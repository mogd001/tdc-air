library(tdcR)
library(zoo)
library(Hilltop)
library(broom)
library(plotly)
library(zeallot)
library(lubridate)
library(glue)

##### Clean BAM (Beta Attenuation Mass/Monitor) Data #####
#
# Matt Ogden, September 2022
#
# Cleans bam data downloaded from 5028i BAM Richmond at Plunket.

# continue from 01_merge_bam_data.R
source("01_merge_bam_data.R")
short
long_5min
long_daily

p_short
p_long_5min
p_long_daily

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
end_date <- gsub("-", "", as.Date(end_datetime) + days(1))

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
disconnect(hts_file)

# get telemetry data
# endpoint <- "http://envdata.tasman.govt.nz/Hydrology-Telemetry.hts?"
# telem_data <- get_data_site_measurement(endpoint = endpoint, site = site, measurement = measurement, from = start_date, to = end_date) %>%
#  rename(pm2p5 = value)

##### Step 1 - comparison to raw telemetry
p1 <- ggplot() +
  geom_line(long_5min, mapping = aes(datetime, pm2p5, color = "downloaded (5 min)")) +
  geom_line(long_30min, mapping = aes(datetime, pm2p5, color = "downloaded (5min -> 30min)")) +
  geom_line(telem_data, mapping = aes(datetime, pm2p5, color = "telemetry (30 min)")) +
  scale_y_continuous(limits = c(-50, NA)) +
  scale_color_manual(values = c("black", "blue", "red")) +
  labs(x = "Datetime", y = "PM2.5", color = "PM2.5 Source") +
  theme(legend.position = "right") +
  theme_bw()


ggplotly(p1)


pm2p5_30min_diff <- telem_data %>%
  left_join(long_30min, by = "datetime", suffix = c("", "_download")) %>%
  transmute(
    datetime,
    pm2p5_30min_diff = pm2p5 - pm2p5_download
  ) %>%
  drop_na()

p2 <- ggplot(pm2p5_30min_diff, mapping = aes(datetime, pm2p5_30min_diff, color = "telemetry - downloaded (30min)")) +
  geom_line() +
  geom_smooth(color = "black", se = FALSE) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(x = "Datetime", y = "PM2.5", color = "Legend") +
  theme(legend.position = "right") +
  theme_bw()

ggplotly(p2)
summary(pm2p5_30min_diff)

##### Step 2 - cleaning
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

clean_long_5min_data <- function(df) {
  # Clean up long data (adding gaps and 0's).
  gaps <- df %>% filter(err == 1 & err_state_change == 1)

  gap_start <- gaps %>%
    select(datetime, err_type, dur) %>%
    transmute(
      datetime = datetime - minutes(5) + seconds(1),
      pm = 0
    )

  gap_end <- gaps %>%
    select(datetime, dur) %>%
    filter(dur > 1) %>%
    mutate(
      datetime = datetime + (dur - 1) * minutes(5),
      pm = 0
    ) %>%
    select(-dur)

  df_cleaned <- df %>%
    filter(!(err == 1 & err_state_change != 1)) %>% # filter error runs excluding start which become "gaps"
    mutate(
      pm = ifelse(err == 1, NA, pm)
    ) %>%
    select(datetime, pm) %>%
    bind_rows(list(gap_start, gap_end)) %>%
    arrange(datetime)

  list(df_cleaned, gaps)
}

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
  write.csv(paste0("outputs/", download_name, "_long_5min_5028i_pm10_cleaned.csv"))

long_5min_pm2p5_cleaned %>%
  mutate_at("pm2p5", as.character) %>%
  replace(is.na(.), "") %>%
  write.csv(paste0("outputs/", download_name, "_long_5min_5028i_pm2p5_cleaned.csv"))

# generate comments from gaps
generate_long_5min_comment <- function(gap) {
  # Generate comments for errors.
  base_string <- "Missing records for duration {duration} from {start_datetime} to {end_datetime} due to {reason}."
  
  gap %>%
    mutate(
      start_datetime = datetime,
      duration = dur * minutes(5), # convert to hours
      end_datetime = datetime + dur * minutes(5),
      comment = ifelse(err_type == 1, glue(base_string, reason = "due to value less than -20"),
                       ifelse(err_type == 2 & duration > minutes(30), glue(base_string, reason = "Watercare undertaking instrument audit"),
                              ifelse(err_type == 2, glue(base_string, reason = "due to value less than -50"),
                                     ifelse(err_type == 3, glue(base_string, reason = "Watercare undertaking instrument audit (power off)"),
                                            "Other error type, please inspect further."
                                     )
                              )
                       )
      )
    )
}

long_5min_pm10_gaps %>%
  generate_long_5min_comment() %>%
  write.csv(paste0("outputs/", download_name, "_long_5min_5028i_pm10_comments.csv"))
long_5min_pm2p5_gaps %>%
  generate_long_5min_comment() %>% 
  write.csv(paste0("outputs/", download_name, "_long_5min_5028i_pm2p5_comments.csv"))

# TODO
# carry on with cleaning procedure as per Matt's email.


# Copy outputs to original directory under merged folder
list_of_files <- list.files("outputs", ".csv") 
file.copy(file.path("outputs", list_of_files), paste0(directory,"/merged"))