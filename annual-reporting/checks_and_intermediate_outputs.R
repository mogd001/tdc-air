library(knitr)
library(tidyverse)
library(lubridate)

aq_day <- readRDS("temp.RDS")

#############
# Checks
check_interval <- interval(start = ymd("20200901"), end = ymd("20210831"))
site <- "AQ Richmond Central at Plunket"
normalising_check <- filter(aq_day, site == !!site & measurement == "PM10") %>%
  mutate(
    year = year(date),
    month = as.numeric(month(date)),
    reporting_year = if_else(month > 8, year + 1, year)
  ) %>%
  filter(date %within% check_interval)

normalising_check_out <- normalising_check %>%
  rename(pm10 = value) %>%
  select(date, pm10, wind_kph_24h_avg, temp_degc_4h_avg, node, normalised_pm) %>%
  mutate(
    exceedance_nodes1to3 = if_else(pm10 > 50 & node < 4, pm10, -999),
    exceedance_nodes1to5 = if_else(pm10 > 50 & node < 6, pm10, -999)
  )

clipr::write_clip(normalising_check_out) # copy to clipboard

#############
# Save Richmond data for reporting
reporting_interval2 <- interval(start = ymd("20210101"), end = ymd("20221006"))

site <- "AQ Richmond Central at Plunket"
aq_day_richmond_pm10_out <- filter(aq_day, site == !!site & measurement == "PM10") %>%
  mutate(
    year = year(date),
    month = as.numeric(month(date)),
    reporting_year = if_else(month > 8, year + 1, year)
  ) %>%
  filter(date %within% reporting_interval2) %>%
  rename(pm10 = value) %>%
  select(date, reporting_year, month, pm10, wind_kph_24h_avg, temp_degc_24h_avg, temp_degc_4h_avg, node, normalised_pm)

clipr::write_clip(aq_day_richmond_pm10_out)

aq_day_richmond_pm10_stats <- group_by(aq_day_richmond_pm10_out, reporting_year, month) %>%
  # aq_day_richmond_pm10_stats <- group_by(aq_day_richmond_pm10_out, reporting_year) %>%
  summarise(
    count = n(),
    count_non_na = sum(!is.na(pm10), na.rm = TRUE),
    count_exceedance = sum(pm10 > 50, na.rm = TRUE),
    min = min(pm10, na.rm = TRUE),
    max = max(pm10, na.rm = TRUE),
    mean = mean(pm10, na.rm = TRUE),
    sd = sd(pm10, na.rm = TRUE),
    median = median(pm10, na.rm = TRUE),
    iqr = IQR(pm10, na.rm = TRUE)
  )
clipr::write_clip(aq_day_richmond_pm10_stats)

aq_day_richmond_pm10_out %>%
  group_by(reporting_year, month) %>%
  summarise(mean = mean(temp_degc_24h_avg, na.rm = TRUE))

aq_day_richmond_pm10_out %>%
  group_by(reporting_year, month) %>%
  summarise(mean = mean(wind_kph_24h_avg, na.rm = TRUE))

aq_day_richmond_pm2p5_out <- filter(aq_day, site == !!site & measurement == "PM2.5") %>%
  mutate(
    year = year(date),
    month = as.numeric(month(date)),
    reporting_year = if_else(month > 8, year + 1, year)
  ) %>%
  filter(date %within% reporting_interval2) %>%
  rename(pm2.5 = value) %>%
  select(date, year, month, pm2.5, wind_kph_24h_avg, temp_degc_24h_avg, temp_degc_4h_avg, node, normalised_pm)

clipr::write_clip(aq_day_richmond_pm2p5_out) # copy to clipboard


aq_day_richmond_pm2p5_stats <- group_by(aq_day_richmond_pm2p5_out, year, month) %>%
  summarise(
    count = n(),
    count_non_na = sum(!is.na(pm2.5), na.rm = TRUE),
    count_exceedance = sum(pm2.5 > 50, na.rm = TRUE),
    min = min(pm2.5, na.rm = TRUE),
    max = max(pm2.5, na.rm = TRUE),
    mean = mean(pm2.5, na.rm = TRUE),
    sd = sd(pm2.5, na.rm = TRUE),
    median = median(pm2.5, na.rm = TRUE),
    iqr = IQR(pm2.5, na.rm = TRUE)
  )
clipr::write_clip(aq_day_richmond_pm2p5_stats)

# aq_day_richmond_pm10_out %>%
#   group_by(year) %>%
#   summarise(mean = median(temp_degc_24h_avg, na.rm = TRUE)) %>%
#   ggplot(aes(year, mean)) +
#   geom_line() +
#   geom_smooth(color = "red") +
#   theme_bw() +
#   labs(x = "year", y = "Richmond average daily temperature (deg C)")







# Save Motueka data for reporting
reporting_interval2 <- interval(start = ymd("20220501"), end = ymd("20220930"))

site <- "AQ Motueka at Goodman Park"
aq_day_motueka_pm2p5_out <- filter(aq_day, site == !!site & measurement == "PM2.5") %>%
  mutate(
    year = year(date),
    month = as.numeric(month(date)),
    reporting_year = if_else(month > 8, year + 1, year)
  ) %>%
  filter(date %within% reporting_interval2) %>%
  rename(pm2.5 = value) %>%
  select(date, year, month, pm2.5, wind_kph_24h_avg, temp_degc_24h_avg, temp_degc_4h_avg, node)

clipr::write_clip(aq_day_motueka_pm2p5_out) # copy to clipboard

aq_day_motueka_pm2p5_out %>%
  group_by(year, month) %>%
  summarise(mean = mean(temp_degc_24h_avg, na.rm = TRUE))

aq_day_motueka_pm2p5_out %>%
  group_by(year, month) %>%
  summarise(mean = mean(wind_kph_24h_avg, na.rm = TRUE))

group_by(aq_day_motueka_pm2p5_out, year) %>%
  summarise(
    count = n(),
    count_non_na = sum(!is.na(pm2.5), na.rm = TRUE),
    count_exceedance = sum(pm2.5 > 50, na.rm = TRUE),
    min = min(pm2.5, na.rm = TRUE),
    max = max(pm2.5, na.rm = TRUE),
    mean = mean(pm2.5, na.rm = TRUE),
    sd = sd(pm2.5, na.rm = TRUE),
    median = median(pm2.5, na.rm = TRUE),
    iqr = IQR(pm2.5, na.rm = TRUE)
  )
