library(tidyverse)
library(lubridate)
library(plotly)
library(hms)
library(patchwork)

##### Merge BAM (Beta Attenuation Mass/Monitor) Data #####
#
# Matt Ogden, September 2022
#
# Merges bam data downloaded from 5028i BAM Richmond at Plunket.
# Short - 30 minute
# Long - 5 minute pm10 and pm2.5 data

# Processing directory (where .dat files are located)
directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(024) 5028i Download 02-08-2022"

##################

# Check if "merged" folder exists in directory
if (dir.exists(paste0(directory,"/merged"))){
  unlink(paste0(directory,"/merged"), recursive = TRUE)
  Sys.sleep(2) # give time for the removal of the directory to take place
} 
dir.create(paste0(directory,"/merged"))

# Clear outputs folder
if (dir.exists("outputs")){
  unlink("outputs", recursive = TRUE)
  Sys.sleep(2) # give time for the removal of the directory to take place
} 
dir.create("outputs")


merge_dat_files <- function(files) {
  # Merge a list of .dat files with equal structure.
  merge_df <- NULL

  for (f in files) {
    df <- read.table(f, head = FALSE, skip = 5)
    names(df) <- df[1, ]
    df <- df[-1, ]

    merge_df <- rbind(merge_df, df)
  }

  rownames(merge_df) <- 1:nrow(merge_df)
  merge_df %>%
    distinct() %>%
    mutate(
      Date = mdy(Date),
      Time = hm(Time),
      datetime = force_tz(Date + Time, tz = "Etc/GMT+12"),
      time = format(datetime, format = "%H:%M:%S")
    ) %>%
    arrange(datetime) %>%
    tibble()
}

# Short and Long files. Short, is if the filename contains "short".

download_name <- lapply(strsplit(directory, "/"), tail, n = 1) %>%  unlist()

filenames <- list.files(directory, pattern = "*.dat", full.names = TRUE)

short_filenames <- filenames[grepl("short", tolower(filenames))]
long_filenames <- filenames[!grepl("short", tolower(filenames))]

day <- format(Sys.time(), "%Y%m%d")

# save to csv
short <- merge_dat_files(short_filenames) %>%
  select(datetime, humidity = ambrh) %>% 
  mutate(humidity = as.numeric(humidity))

#short %>%
#  write.csv(paste0("outputs/", download_name, "_short_5028i.csv"))

long <- merge_dat_files(long_filenames) %>%
  mutate(
    pm = as.numeric(pm),
    pmb = as.numeric(pmb),
    avgpm = as.numeric(avgpm),
    avgpmb = as.numeric(avgpmb)
  )

long_5min <- long %>%
  select(datetime, pm10 = pm, pm2p5 = pmb)
#long_5min %>%
#  write.csv(paste0("outputs/", download_name, "_long_5min_5028i.csv"))

long_daily <- long %>%
  select(-c(Time, Date)) %>% 
  mutate(datetime = datetime - minutes(5), #offset datetime by 5 minutes
         Date = as.Date(datetime),
         Time = as_hms(datetime)) %>% 
  group_by(Date) %>%
  mutate(pm10_daily = ifelse(any(avgpm) > 1000 , 9999, avgpm),
         pm2p5_daily = ifelse(any(avgpmb) > 1000, 9999, avgpmb),
         pm10_daily_calc = mean(pm, na.rm = TRUE), 
         pm2p5_daily_calc = mean(pmb, na.rm = TRUE),
         ) %>%
  ungroup() %>%
  filter(Time != parse_time("23:55:00")) %>%
  select(Date, pm10_daily, pm2p5_daily, pm10_daily_calc, pm2p5_daily_calc) %>%
  mutate_if(is.numeric, round, 3) %>% 
  distinct() %>%
  mutate(
    pm10_daily_calc = lag(pm10_daily_calc),
    pm2p5_daily_calc = lag(pm2p5_daily_calc),
    datetime = Date - days(1) + hours(23) + minutes(59) + seconds(59)
  ) %>%
  select(datetime, pm10_daily, pm2p5_daily, pm10_daily_calc, pm2p5_daily_calc)

#long_daily %>%
#  write.csv(paste0("outputs/", download_name, "_long_daily_5028i.csv"))

# visualise for testing
p_short <- ggplot(short, aes(datetime, humidity)) +
  geom_point(size = 0.7, color = "blue") +
  labs(x = "", y = "Humidity (%)") + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_datetime(date_labels = "%Y-%b") +
  theme_bw()

p1 <- ggplot(long_5min, aes(datetime, pm10)) +
  geom_point(size = 0.7, color = "red") +
  labs(x = "", y = "PM10 (ug/m3)") + 
  scale_y_continuous(limits = c(0, max(long_5min$pm10) * 1.05), expand = c(0, 0)) + 
  scale_x_datetime(date_labels = "%Y-%b") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
  
p2 <- ggplot(long_5min, aes(datetime, pm2p5)) +
  geom_point(size = 0.7, color = "orange") +
  labs(x = "", y = "PM2.5 (ug/m3)") + 
  scale_y_continuous(limits = c(0, max(long_5min$pm10) * 1.05), expand = c(0, 0)) + 
  scale_x_datetime(date_labels = "%Y-%b") + 
  theme_bw()

p_long_5min <- p1 / p2 

p_long_daily <- ggplot(long_daily)  +
  geom_line(mapping = aes(datetime, pm10_daily, color = "PM10"), size = 0.7) +
  geom_line(mapping = aes(datetime, pm2p5_daily, color = "PM2.5"), size = 0.7) +
  labs(x = "", y = "PM (ug/m3)") + 
  scale_color_manual(name = "Legend", values = c("PM10" = "red", "PM2.5" = "orange")) + 
  scale_x_datetime(date_labels = "%Y-%b") + 
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) + 
  theme_bw()

rm(long)