library(zoo)
library(Hilltop)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(openair)

################################################################################
# Load aq data
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
  filter(date >= "2021-05-15" & date <= "2021-08-04") %>%
  drop_na() %>% 
  mutate(
    is_target_range  = ifelse(date >= "2021-06-01" & date <= "2021-06-05", TRUE, FALSE),
    is_target_date = ifelse(date == "2021-06-03", TRUE, FALSE)
    ) 

day_riwaka_east_df <- riwaka_east_df %>% 
  group_by(date) %>%
  summarise(mean_pm2p5 = mean(pm2p5, na.rm = TRUE)) %>%
  mutate(is_target_date = ifelse(date == "2021-06-03", TRUE, FALSE))
label_pos <- max(day_riwaka_east_df$date)-7
peak_daily_pm2p5 <- round(max(day_riwaka_east_df$mean_pm2p5), 2)
  
# 24 hour daily average plot
p1 <- ggplot(day_riwaka_east_df, aes(x = date, y = mean_pm2p5)) + 
  geom_col(aes(fill = is_target_date), show.legend = FALSE) + 
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_text(aes(label_pos, 15, label = "WHO 2021 guideline", vjust = -1), size = 3, color = "red") +
  geom_hline(yintercept = 25, color = "red") +
  geom_text(aes(label_pos, 25, label = "MFE proposed standard", vjust = -1), size = 3, color = "red") +
  labs(title = paste0("AQ Riwaka at Riwaka East PM2.5 24hr Average [2021]"), x = "Date", y = "PM2.5 (ug/m3)") +
  theme_classic() +
  scale_fill_manual(values=c("#273691", "orange")) +
  scale_x_date(limits = c(min(day_riwaka_east_df$date)+4, max(day_riwaka_east_df$date))-2, breaks = date_breaks("5 day"), minor_breaks = date_breaks("1 day"), labels = date_format("%d %b")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,30)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Box and whisker plots
box1_df <- riwaka_east_df %>%
  filter(is_target_range)

p2 <- ggplot(box1_df) +
  geom_boxplot(aes(x = date, y = pm2p5, group = date)) + 
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 25, color = "red") +
  scale_x_date(breaks = date_breaks("1 day"), minor_breaks = date_breaks("1 day"), 
               labels = date_format("%d %b")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = "Box and Whisker - Daily Comparison",
       x = "", y = "PM2.5 (ug/m3)", fill = "Site") + 
  facet_wrap(~ site, ncol = 1)

box2_df <- riwaka_east_df %>%
  filter(weekday == 4)

p3<- ggplot(box2_df) +
  geom_boxplot(aes(x = is_target_date, y = pm2p5)) + 
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 25, color = "red") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels=c("FALSE" = "Thurs 03/06", "TRUE" = "Thurs Other")) +
  labs(title = "Box and Whisker - Comparison of Week Day",
       x = "", y = "PM2.5 (ug/m3)") + 
  facet_wrap(~ site, ncol = 1)

# Timeseries plot
ts_df <- riwaka_east_df %>%
  filter(is_target_range == TRUE)
scaleFactor <- max(ts_df$pm2p5) / max(ts_df$temp)

p4 <- ggplot(ts_df, aes(x = datetime)) +
  geom_line(aes(y = pm2p5), col="blue")+ 
  geom_line(aes(y = temp), col="red") +
  annotate("rect", xmin = as.POSIXct("2021-06-03 00:00:00"), xmax = as.POSIXct("2021-06-03 23:59:59"), ymin = 0, ymax = 150,
           alpha = .1,fill = "orange") +
  scale_y_continuous(
    name = "PM2.5 (ug/m3)",
    limits = c(0,150),
    sec.axis = sec_axis(~./scaleFactor, name="Temperature (deg C)")
  ) +
  scale_x_datetime(breaks = date_breaks("1 day"), minor_breaks = date_breaks("1 day"), labels = date_format("%d %b", tz = "NZ"), name = " ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  )

# Windrose plot
wr1_df <- riwaka_east_df %>%
  filter(is_target_date == TRUE)
  
p5 <- windRose(wr1_df, ws = "wind_speed", wd = "wind_dir",
         breaks = c(1, 5, 10, 15, 20, 25, 999),
         angle = 45,
         paddle = FALSE,
         width = 2.0,
         auto.text = FALSE,
         annotate = TRUE,
         grid.line = 5,
         key = list(labels = c("1-5","5-10","10-15","15-20","20-25",">25"), header="Windrose Plot ", footer="Wind Speed (km/h)",
                    plot.style = c("ticks", "border"), fit = "all", height = 1, space = "top"),
         par.settings = list(axis.line=list(col="lightgray")),
         col = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"))

wr2_df <- riwaka_east_df %>%
  filter(is_target_date == FALSE)

p6 <- windRose(wr2_df, ws = "wind_speed", wd = "wind_dir",
               breaks = c(1, 5, 10, 15, 20, 25, 999),
               angle = 45,
               paddle = FALSE,
               width = 2.0,
               auto.text = FALSE,
               annotate = TRUE,
               grid.line = 5,
               key = list(labels = c("1-5","5-10","10-15","15-20","20-25",">25"), header="Windrose Plot ", footer="Wind Speed (km/h)",
                          plot.style = c("ticks", "border"), fit = "all", height = 1, space = "top"),
               par.settings = list(axis.line=list(col="lightgray")),
               col = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"))
