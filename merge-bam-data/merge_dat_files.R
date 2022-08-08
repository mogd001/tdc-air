library(tidyverse)
library(lubridate)
library(plotly)

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
      datetime = Date + Time,
      Time = format(datetime, format = "%H:%M:%S")
    ) %>%
    arrange(datetime) %>%
    tibble()
}

# Short and Long files. Short, is if the filename contains "short".
directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(024) 5028i Download 02-08-2022"
filenames <- list.files(directory, pattern = "*.dat", full.names = TRUE)

short_filenames <- filenames[grepl("short", tolower(filenames))]
long_filenames <- filenames[!grepl("short", tolower(filenames))]

day <- format(Sys.time(), "%Y%m%d")

# save to csv
short <- merge_dat_files(short_filenames) %>%
  select(datetime, humidity = ambrh)
short %>%
  write.csv(paste0("outputs/", day, "_short_5028i.csv"))
# only output selected columns

long <- merge_dat_files(long_filenames) %>%
  mutate(
    pm = as.numeric(pm),
    pmb = as.numeric(pmb),
    avgpm = as.numeric(avgpm),
    avgpmb = as.numeric(avgpmb)
  )

long_5min <- long %>%
  select(datetime, pm10 = pm, pm2p5 = pmb)
long_5min %>%
  write.csv(paste0("outputs/", day, "_long_5min_5028i.csv"))

long_daily <- long %>%
  group_by(Date) %>%
  mutate(pm10_daily_calc = mean(pm, na.rm = TRUE), pm2p5_daily_calc = mean(pmb, na.rm = TRUE)) %>%
  ungroup() %>%
  subset(Time != "00:00:00") %>%
  select(Date, pm10_daily = avgpm, pm2p5_daily = avgpmb, pm10_daily_calc, pm2p5_daily_calc) %>%
  distinct() %>%
  mutate(
    pm10_daily_calc = lag(pm10_daily_calc),
    pm2p5_daily_calc = lag(pm2p5_daily_calc),
    datetime = Date - days(1) + hours(23) + minutes(59) + seconds(59)
  ) %>%
  select(datetime, pm10_daily, pm2p5_daily, pm10_daily_calc, pm2p5_daily_calc)

long_daily %>%
  write.csv(paste0("outputs/", day, "_long_daily_5028i.csv"))

# visualise for testing
short <- short %>%
  mutate(humidity = as.numeric(humidity))

p_short <- ggplot(short, aes(datetime, humidity)) +
  geom_point(size = 1, color = "blue") +
  scale_y_continuous(limits = c(0, NA))

p_long <- ggplot(long_5min, aes(datetime, pm10)) +
  geom_point(size = 1, color = "red") +
  scale_y_continuous(limits = c(0, NA))

ggplotly(p_short)
ggplotly(p_long)
