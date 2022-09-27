library(tdcR)
library(zoo)
library(Hilltop)
library(broom)
library(plotly)
library(zeallot)
library(lubridate)
library(glue)
library(patchwork)
library(knitr)

source("functions.R")

##### Clean BAM (Beta Attenuation Mass/Monitor) Data #####
#
# Matt Ogden, September 2022
#
# Cleans bam data downloaded from 5028i BAM Richmond Central at Plunket.

# continue from 01_merge_bam_data.R
source("1_merge_bam_data.R")
short
long_5min
long_daily

long_30min <- long_5min %>%
  mutate(datetime = floor_date(long_5min$datetime, "30 minutes") + minutes(30)) %>%
  group_by(datetime) %>%
  summarise(
    pm10 = mean(pm10, na.rm = TRUE),
    pm2p5 = mean(pm2p5, na.rm = TRUE)
  )

start_datetime <- min(long_5min$datetime)
end_datetime <- max(long_5min$datetime)

start_date <- gsub("-", "", as.Date(start_datetime))
end_date <- gsub("-", "", as.Date(end_datetime))

# raw telemetry
hts_file <- HilltopData(r"{\\tsrvfiles\hydrology\Datafiles\RawTelem_Concatenated.dsn}")

site <- "AQ Richmond Central at Plunket"
measurement <- "PM2.5 (30min) [PM2.5 (30min) TDC 5028i BAM]"

telem_data <- GetData(hts_file, site, measurement, start_date, end_date) %>%
  tidy() %>%
  rename(
    datetime = index,
    pm2p5 = value
  )

telem_data$datetime <- force_tz(telem_data$datetime , tz = "Etc/GMT+12")

disconnect(hts_file)

# get telemetry data
# endpoint <- "http://envdata.tasman.govt.nz/Hydrology-Telemetry.hts?"
# telem_data <- get_data_site_measurement(endpoint = endpoint, site = site, measurement = measurement, from = start_date, to = end_date) %>%
#  rename(pm2p5 = value)

##### Step 1 - comparison to raw telemetry
p_rawtelemetry <- ggplot() +
  geom_line(long_5min, mapping = aes(datetime, pm2p5, color = "downloaded (5 min)")) +
  geom_line(long_30min, mapping = aes(datetime, pm2p5, color = "downloaded (5min -> 30min)")) +
  geom_line(telem_data, mapping = aes(datetime, pm2p5, color = "telemetry (30 min)")) +
  scale_y_continuous(limits = c(-50, NA)) +
  scale_color_manual(values = c("black", "blue", "red")) +
  labs(x = "Datetime", y = expression(PM2.5~(mu*g/m^{3})), color = "PM2.5 Source") +
  theme(legend.position = "right") +
  theme_bw()

ggsave("outputs/telemetry_comparison.png", plot = p_rawtelemetry, width = 12)
#ggplotly(p1)

pm2p5_30min_diff <- telem_data %>%
  left_join(long_30min, by = "datetime", suffix = c("", "_download")) %>%
  transmute(
    datetime,
    pm2p5_30min_diff = pm2p5 - pm2p5_download
  ) %>%
  drop_na()

p_telemetrycomparison <- ggplot(pm2p5_30min_diff, mapping = aes(datetime, pm2p5_30min_diff, color = "telemetry - downloaded (30min)")) +
  geom_line() +
  geom_smooth(color = "black", se = FALSE) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(x = "Datetime", y = expression(PM2.5~(mu*g/m^{3})), color = "Legend") +
  theme(legend.position = "right") +
  theme_bw()

#ggplotly(p_telemetrycomparison)
summary(p_telemetrycomparison)

##### Step 2 - cleaning
##### Long 5 Minute
long_5min_with_err <- tibble(datetime = seq(start_datetime, end_datetime, by = "5 min")) %>% # make sure we have a complete initial 5 minute dataset between the start and end dates.
  left_join(long_5min, by = "datetime") %>%
  pivot_longer(
    cols = c("pm10", "pm2p5"),
    names_to = "name",
    values_to = "value"
  ) %>%
  group_by(name) %>%
  mutate(
    value_err_type = ifelse(is.na(value), 3,
      ifelse(value <= -50, 2,
        ifelse(value <= -20, 1, 0)
      )
    ),
    value_err = ifelse(value_err_type > 0, 1, 0)
  ) %>%
  pivot_wider(
    names_from = name,
    values_from = c(value, value_err, value_err_type),
    names_glue = "{name}_{.value}"
  ) %>%
  rename_with(~ sub("_value", "", .x), everything())


# PM10
long_5min_pm10 <- long_5min_with_err %>%
  select(datetime, pm10, pm10_err, pm10_err_type) %>%
  transmute( # consolidate error columns
    datetime,
    pm = pm10,
    err = pm10_err,
    err_state_change = err - lag(err),
    dur = rep(rle(err == 1)$values * rle(err == 1)$lengths, rle(err == 1)$lengths),
    err_type = pm10_err_type
  )

# PM2p5
long_5min_pm2p5 <- long_5min_with_err %>%
  select(datetime, pm2p5, pm2p5_err, pm2p5_err_type) %>%
  transmute( # consolidate error columns
    datetime,
    pm = pm2p5,
    err = pm2p5_err,
    err_state_change = err - lag(err),
    dur = rep(rle(err == 1)$values * rle(err == 1)$lengths, rle(err == 1)$lengths),
    err_type = pm2p5_err_type
  )

c(long_5min_pm10_cleaned, long_5min_pm10_gaps) %<-% clean_long_5min_data(long_5min_pm10)

long_5min_pm10_cleaned <- long_5min_pm10_cleaned %>%
  rename(pm10 = pm)
long_5min_pm10_gaps <- long_5min_pm10_gaps %>%
  rename(pm10 = pm)

c(long_5min_pm2p5_cleaned, long_5min_pm2p5_gaps) %<-% clean_long_5min_data(long_5min_pm2p5)

long_5min_pm2p5_cleaned <- long_5min_pm2p5_cleaned %>%
  rename(pm2p5 = pm)
long_5min_pm2p5_gaps <- long_5min_pm2p5_gaps %>%
  rename(pm2p5 = pm)

# tidy up NAs to "" for export
long_5min_pm10_cleaned %>%
  mutate_at("pm10", as.character) %>%
  replace(is.na(.), "") %>%
  mutate(datetime = format(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  write_csv(paste0("outputs/", download_name, "_long_5min_5028i_pm10_cleaned.csv"))

long_5min_pm2p5_cleaned %>%
  mutate_at("pm2p5", as.character) %>%
  replace(is.na(.), "") %>%
  mutate(datetime = format(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  write_csv(paste0("outputs/", download_name, "_long_5min_5028i_pm2p5_cleaned.csv"))

comments_5min_pm10 <- long_5min_pm10_gaps %>%
  generate_long_5min_comments()
comments_5min_pm10 %>% write_csv(paste0("outputs/", download_name, "_long_5min_5028i_pm10_comments.csv"))

comments_5min_pm2p5 <- long_5min_pm2p5_gaps %>%
  generate_long_5min_comments()
comments_5min_pm2p5 %>% write_csv(paste0("outputs/", download_name, "_long_5min_5028i_pm2p5_comments.csv"))

# Generate graph comparing raw vs processed 5 minute data
long_5min
p_long_5min

long_5min_cleaned <- long_5min_pm10_cleaned %>%
  left_join(long_5min_pm2p5_cleaned, by = "datetime")

p3 <- ggplot(long_5min_cleaned, aes(datetime, pm10)) +
  geom_point(size = 0.7, color = "red") +
  labs(x = "", y = expression(PM10~(mu*g/m^{3})), title = "PM10 Cleaned (5 minute)") +
  scale_y_continuous(limits = c(-50, max(long_5min$pm10) * 1.05), expand = c(0, 0)) +
  scale_x_datetime(date_labels = "%Y-%b") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p4 <- ggplot(long_5min_cleaned, aes(datetime, pm2p5)) +
  geom_point(size = 0.7, color = "orange") +
  labs(x = "", y = expression(PM2.5~(mu*g/m^{3})), title = "PM2.5 Cleaned (5 minute)") +
  scale_y_continuous(limits = c(-50, max(long_5min$pm2p5) * 1.05), expand = c(0, 0)) +
  scale_x_datetime(date_labels = "%Y-%b") +
  theme_bw()

p_long_5min_comparison <- (p1 / p2) | (p3 / p4)
ggsave("outputs/long_5min_comparison.png", plot = p_long_5min_comparison, width = 12)

##### Long Daily

# PM10
long_daily_pm10 <- long_daily %>%
  transmute(
    datetime,
    date,
    pm = pm10_daily,
    pm_calc = pm10_daily_calc
  )

df_5min_cleaned <- long_5min_pm10_cleaned %>% rename(pm = pm10)
gaps <- long_5min_pm10_gaps %>%
  rename(pm = pm10) %>%
  select(datetime, dur) %>%
  mutate(date = as.Date(datetime, tz = "Etc/GMT+12")) %>% 
  group_by(date) %>% 
  summarise(date = date, dur = sum(dur)) %>% 
  ungroup() %>% 
  unique()

c(long_daily_pm10_cleaned, gaps_pm10) %<-% clean_long_daily_data(long_daily_pm10, df_5min_cleaned, gaps)
long_daily_pm10_cleaned <- long_daily_pm10_cleaned %>%
  rename(pm10 = pm)

long_daily_pm10_cleaned %>%
  mutate(pm10 = round(pm10, 0)) %>% 
  mutate_at("pm10", as.character) %>%
  replace(is.na(.), "") %>%
  mutate(datetime = format(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  mutate(datetime = ifelse(pm10 == "", "gap", datetime)) %>% 
  mutate(pm10 = ifelse(datetime == "gap", "gap", pm10)) %>% 
  write_csv(paste0("outputs/", download_name, "_long_daily_5028i_pm10_cleaned.csv"))

comments_daily_pm10 <- gaps_pm10 %>%
  generate_long_daily_comments()
comments_daily_pm10 %>% write_csv(paste0("outputs/", download_name, "_long_daily_5028i_pm10_comments.csv"))

# PM2p5
long_daily_pm2p5 <- long_daily %>%
  transmute(
    datetime,
    date,
    pm = pm2p5_daily,
    pm_calc = pm2p5_daily_calc
  )

df_5min_cleaned <- long_5min_pm2p5_cleaned %>% rename(pm = pm2p5)
gaps <- long_5min_pm2p5_gaps %>%
  rename(pm = pm2p5) %>%
  select(datetime, dur) %>%
  mutate(date = as.Date(datetime, tz = "Etc/GMT+12")) %>% 
  group_by(date) %>% 
  summarise(date = date, dur = sum(dur)) %>% 
  ungroup() %>% 
  unique()

c(long_daily_pm2p5_cleaned, gaps_pm2p5) %<-% clean_long_daily_data(long_daily_pm2p5, df_5min_cleaned, gaps)
long_daily_pm2p5_cleaned <- long_daily_pm2p5_cleaned %>%
  rename(pm2p5 = pm)

long_daily_pm2p5_cleaned %>%
  mutate(pm2p5 = round(pm2p5, 1)) %>% 
  mutate_at("pm2p5", as.character) %>%
  replace(is.na(.), "") %>%
  mutate(datetime = as.character(format(datetime, format="%Y-%m-%d %H:%M:%S"))) %>% 
  mutate(datetime = ifelse(pm2p5 == "", "gap", datetime)) %>% 
  mutate(pm2p5 = ifelse(datetime == "gap", "gap", pm2p5)) %>% 
  write_csv(paste0("outputs/", download_name, "_long_daily_5028i_pm2p5_cleaned.csv"))

comments_daily_pm2p5 <- gaps_pm2p5 %>%
  generate_long_daily_comments() 
comments_daily_pm2p5 %>% write_csv(paste0("outputs/", download_name, "_long_daily_5028i_pm2p5_comments.csv"))

# Modify for daily plotting and reporting
d1 <- long_daily_pm10_cleaned %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date) %>% 
  summarise(date = date, pm10 = sum(pm10))
  
d2 <- long_daily_pm2p5_cleaned %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date) %>% 
  summarise(date = date, pm2p5 = sum(pm2p5))

p_long_daily_cleaned <- ggplot() +
  geom_step(data = d1, aes(date, pm10, color = "PM10"), size = 0.5) +
  geom_step(data = d2, aes(date, pm2p5, color = "PM2.5"), size = 0.5) +
  labs(x = "", y = expression(PM~(mu*g/m^{3})), title = "PM Daily Cleaned") +
  scale_color_manual(name = "Legend", values = c("PM10" = "red", "PM2.5" = "orange")) +
  scale_x_date(date_labels = "%Y-%b") +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 1)) +
  theme_bw()

ggsave("outputs/long_daily_cleaned.png", plot = p_long_daily_cleaned, width = 12)

p_long_daily_comparison <- p_long_daily_raw | p_long_daily_cleaned
ggsave("outputs/long_daily_comparison.png", plot = p_long_daily_comparison, width = 12)

# Final plots and outputs
# List missing days, list recalculated days, list max and min with dates, save to text file
write_lines("PM10 Daily Summary", file("outputs/daily_summary.txt"))
write_lines("Errors", file("outputs/daily_summary.txt"), append = TRUE)

gaps_pm10 %>% 
  mutate(duration = dur * minutes(5)) %>% 
  rename(
    pm_raw = pm, 
    pm_cleaned = pm_out
  ) %>% 
  select(date, duration, pm_raw, pm_cleaned) %>% 
  kable() %>% 
  write_lines(file("outputs/daily_summary.txt"), append = TRUE) 

write_lines("Summary", file("outputs/daily_summary.txt"), append = TRUE)
write_lines(kable(summary(d1 %>% filter(pm10 > 0))), file("outputs/daily_summary.txt"), append = TRUE)
write_lines("", file("outputs/daily_summary.txt"), append = TRUE)
write_lines("PM2.5 Daily Summary", file("outputs/daily_summary.txt"), append = TRUE)
write_lines("Errors", file("outputs/daily_summary.txt"), append = TRUE)

gaps_pm2p5 %>% 
  mutate(duration = dur * minutes(5)) %>% 
  rename(
    pm_raw = pm, 
    pm_cleaned = pm_out
  ) %>% 
  select(date, duration, pm_raw, pm_cleaned) %>% 
  kable() %>% 
  write_lines(file("outputs/daily_summary.txt"), append = TRUE) 

write_lines("Summary", file("outputs/daily_summary.txt"), append = TRUE)
write_lines(kable(summary(d2 %>% filter(pm2p5 > 0))), file("outputs/daily_summary.txt"), append = TRUE)

# Save short data
short %>% 
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>% 
  mutate(datetime = format(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  write_csv(paste0("outputs/", download_name, "_short_30min_5028i.csv"))

# Copy outputs to original directory under merged folder
list_of_files <- list.files("outputs", ".csv|.png|.txt")
file.copy(file.path("outputs", list_of_files), paste0(directory, "/merged"))

# Insert comments into sql database directly.
# comments_5min_pm10 %>% insert_comments_to_envmon(measurement = "5min_pm10")
# comments_5min_pm2p5 %>% insert_comments_to_envmon(measurement = "5min_pm2p5")
# comments_daily_pm10 %>% 
#   mutate(datetime = date + hours(12)) %>% 
#   insert_comments_to_envmon(measurement = "daily_pm10")
# comments_daily_pm2p5 %>% 
#   mutate(datetime = date + hours(12)) %>% 
#   insert_comments_to_envmon(measurement = "daily_pm2p5")
  
# TODO
# Insert into hts file directly - to explore further.
  
