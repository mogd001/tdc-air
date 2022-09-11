library(zoo)
library(Hilltop)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(openair)

################################################################################
# Load air quality data
data <- HilltopData(r"{\\tsrvfiles\hydrology\Staff Folders\matto\Riwaka AQ.hts}")

sites <- SiteList(data)
sites <- sites[!grepl("Goodman Park", sites)]

aq_df <- tibble()
for (site in sites) {
  res <- GetData(data, site, "PM2.5 (1min) [PM2.5 (1min) Mote ES642]", "", "") %>%
    as.data.frame() %>%
    rename("pm2p5" = ".") %>%
    cbind(datetime = rownames(.)) %>% 
    as.data.frame(row.names = 1:nrow(.)) %>%
    mutate(
      datetime = as.POSIXct(.$datetime, format="%Y-%m-%d %H:%M:%S", tz= "NZ"),
      site = site,
      date = as.Date(datetime, tz= "NZ"),
      hour = hour(datetime),
      date_hour = floor_date(datetime, "1 hour"),
      weekday = wday(datetime, week_start = 1)
    ) %>%
    as_tibble()
  aq_df <- rbind(aq_df, res)
}

# Load wind and temp data
temp <- GetData(data, "AQ Riwaka at Riwaka North", "Air Temperature (continuous) [Air Temperature (continuous)]", "", "") %>%
  as.data.frame() %>%
  rename("temp" = ".") %>%
  cbind(datetime = rownames(.)) %>% 
  as.data.frame(row.names = 1:nrow(.)) %>%
  mutate(
    datetime = as.POSIXct(.$datetime, format="%Y-%m-%d %H:%M:%S"),
  ) %>%
  as_tibble()

wind_speed <- GetData(data, "AQ Riwaka at Riwaka East", "Wind Speed [Wind Speed]", "", "") %>%
  as.data.frame() %>%
  rename("wind_speed" = ".") %>%
  cbind(datetime = rownames(.)) %>% 
  as.data.frame(row.names = 1:nrow(.)) %>%
  mutate(
    datetime = as.POSIXct(.$datetime, format="%Y-%m-%d %H:%M:%S"),
  ) %>%
  as_tibble()

wind_dir <- GetData(data, "AQ Riwaka at Riwaka East", "Wind Direction [Wind Direction]", "", "") %>%
  as.data.frame() %>%
  rename("wind_dir" = ".") %>%
  cbind(datetime = rownames(.)) %>% 
  as.data.frame(row.names = 1:nrow(.)) %>%
  mutate(
    datetime = as.POSIXct(.$datetime, format="%Y-%m-%d %H:%M:%S"),
  ) %>%
  as_tibble()

# Join temp, wind_speed and wind_dir to create "meteo_df"
meteo_df <- tibble(
  datetime = seq(
    ymd_hms(paste(as.Date(head(temp$datetime,1), tz = "NZ"), "00:00:00", sep=" ")), ymd_hms(paste(as.Date(tail(temp$datetime,1), tz = "NZ"), "00:00:00", sep=" ")),
    by = "1 mins")
) %>%
  mutate(
    hour = hour(datetime),
    weekday = wday(datetime, week_start = 1),
    date_hour = floor_date(datetime, "1 hour"),
    date = as.Date(datetime)
  ) %>%
  left_join(temp[, c("datetime", "temp")], by = "datetime") %>%
  left_join(wind_speed[, c("datetime", "wind_speed")], by = "datetime") %>%
  left_join(wind_dir[, c("datetime", "wind_dir")], by = "datetime") %>%
  drop_na()

disconnect(data)
################################################################################

################################################################################
# Visualisation
riwaka_east_df <- aq_df %>%
  subset(site == "AQ Riwaka at Riwaka East") %>%
  left_join(meteo_df[, c("datetime", "temp", "wind_speed", "wind_dir")], by = "datetime") %>%
  filter(date >= "2021-05-15" & date <= "2021-09-01") %>%
  mutate(
    is_target_range  = ifelse(date >= "2021-06-01" & date <= "2021-06-05", TRUE, FALSE),
    is_target_date = ifelse(date == "2021-06-03", TRUE, FALSE)
  ) 

# 24 hour daily average plot
day_riwaka_east_df <- riwaka_east_df %>% 
  group_by(date) %>%
  summarise(pm2p5_mean = mean(pm2p5, na.rm = TRUE),
            temp_mean = mean(temp, na.rm = TRUE)) %>%
  mutate(is_target_date = ifelse(date == "2021-06-03", TRUE, FALSE))
label_pos <- max(day_riwaka_east_df$date)-7
peak_daily_pm2p5 <- round(max(day_riwaka_east_df$pm2p5_mean), 2)

p1 <- ggplot(day_riwaka_east_df, aes(x = date, y = pm2p5_mean)) + 
  geom_col(aes(fill = is_target_date), show.legend = FALSE) + 
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_text(aes(label_pos, 15, label = "WHO 2021 guideline", vjust = -1), size = 3, color = "red") +
  geom_hline(yintercept = 25, color = "red") +
  geom_text(aes(label_pos, 25, label = "MFE proposed standard", vjust = -1), size = 3, color = "red") +
  labs(title = "AQ Riwaka East PM2.5 24hr Average [2021]", x = "Date", y = "PM2.5 (ug/m3)", color = "Legend") +
  theme_classic() +
  scale_fill_manual(values=c("blue", "orange")) +
  scale_x_date(limits = c(min(day_riwaka_east_df$date)+4, max(day_riwaka_east_df$date))-2, breaks = date_breaks("5 day"), minor_breaks = date_breaks("1 day"), labels = date_format("%d %b")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,30)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1

# Diurnal plot
diurnal_pm2p5_alldates <- riwaka_east_df %>% 
  group_by(hour) %>% 
  summarise(pm2p5_mean = mean(pm2p5, na.rm = TRUE),
            pm2p5_median = median(pm2p5, na.rm = TRUE),
            pm2p5_85th = quantile(pm2p5, 0.85, na.rm = TRUE)
  )
diurnal_pm2p5_targetdate <- riwaka_east_df %>% 
  filter(is_target_date == TRUE) %>% 
  group_by(hour) %>% 
  summarise(pm2p5_mean = mean(pm2p5, na.rm = TRUE),
            pm2p5_median = median(pm2p5, na.rm = TRUE),
            pm2p5_85th = quantile(pm2p5, 0.85, na.rm = TRUE)
  )

colors <- c("3rd June 2021" = "orange", "Winter 2021" = "blue")
p2 <- ggplot() +
  geom_line(data = diurnal_pm2p5_targetdate, aes(x = hour, y = pm2p5_mean, color = "3rd June 2021")) +
  geom_line(data = diurnal_pm2p5_alldates, aes(x = hour, y = pm2p5_mean, color = "Winter 2021")) + 
  #geom_line(data = diurnal_pm2p5_alldates, aes(x = hour, y = pm2p5_85th), color = "#273691", linetype = 2) + 
  #geom_line(data = diurnal_pm2p5_targetdate, aes(x = hour, y = pm2p5_85th), color = "orange", linetype = 2) +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_text(aes(22, 15, label = "WHO 2021 guideline", vjust = -1), size = 3, color = "red") +
  geom_hline(yintercept = 25, color = "red") +
  geom_text(aes(22, 25, label = "MFE proposed standard", vjust = -1), size = 3, color = "red") +
  labs(title = "AQ Riwaka East PM2.5 Hourly Average Comparison", x = "Hour", y = "PM2.5 (ug/m3)", color = "Legend") +
  scale_color_manual(values = colors) + 
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 24)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50))

# Temperature plot
not_target_day <- day_riwaka_east_df %>%  
  filter(is_target_date == FALSE)
target_day <- day_riwaka_east_df %>%  
  filter(is_target_date == TRUE)

p3 <- ggplot() +
  geom_point(data = target_day, aes(x = temp_mean, y = pm2p5_mean, color = "3rd June 2021"), size = 2) + 
  geom_point(data = not_target_day, aes(x = temp_mean, y = pm2p5_mean, color = "Winter 2021"), size = 2) +
  labs(title = "AQ Riwaka East PM2.5 vs Temperature Daily Average Comparison", x = "Temperature (deg C)", y = "PM2.5 (ug/m3)", color = "Legend") +
  scale_color_manual(values = colors) + 
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), limits = c(0,20)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20))

wr1_df <- riwaka_east_df %>%
  filter(is_target_date == TRUE)

p4 <- windRose(wr1_df, ws = "wind_speed", wd = "wind_dir",
               breaks = c(1, 5, 10, 15, 20, 25, 999),
               angle = 45,
               paddle = FALSE,
               width = 2.0,
               auto.text = FALSE,
               annotate = TRUE,
               grid.line = 5,
               key = list(labels = c("1-5","5-10","10-15","15-20","20-25",">25"), header="3rd June 2021", footer="Wind Speed (km/h)",
                          plot.style = c("ticks", "border"), fit = "all", height = 1, space = "top"),
               par.settings = list(axis.line=list(col="lightgray")),
               col = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"))

p5 <- windRose(riwaka_east_df, ws = "wind_speed", wd = "wind_dir",
               breaks = c(1, 5, 10, 15, 20, 25, 999),
               angle = 45,
               paddle = FALSE,
               width = 2.0,
               auto.text = FALSE,
               annotate = TRUE,
               grid.line = 5,
               key = list(labels = c("1-5","5-10","10-15","15-20","20-25",">25"), header="Winter 2021", footer="Wind Speed (km/h)",
                          plot.style = c("ticks", "border"), fit = "all", height = 1, space = "top"),
               par.settings = list(axis.line=list(col="lightgray")),
               col = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"))
