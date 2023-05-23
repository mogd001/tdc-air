library(tidyverse)
library(lubridate)
library(tdcR)

get_aq_data <- function(site,
                        measurement) {
  # Get air quality data from Hilltop Server.
  get_data_site_measurement(site = site, measurement = measurement, interval = "1 day", from = "Data start", to = "Data end") %>%
    mutate(date = as.Date(datetime, tz = "NZ")) %>% 
    group_by(date) %>%
    summarise(
      value = max(value),
    ) %>%
    filter(value > 0)
}

site <- "AQ Motueka at Goodman Park"
measurements <- get_measurements(site = site)

aqr_pm2p5_day <- get_aq_data(site = site, measurement = "PM2.5 (24hr) [PM2.5 (24hr) TDC Partisol]") %>%
  mutate(measurement = "PM2.5", site = site)
aqr_pm10_day <- get_aq_data(site = site, measurement = "PM10 (24hr) [PM10 (24hr) TDC 2025i Partisol]") %>%
  mutate(measurement = "PM10", site = site)

aqr_pm2p5_day %>%
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(count_days = n()) %>% 
  clipr::write_clip()

aqr_pm10_day %>%
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(count_days = n())  %>% 
  clipr::write_clip()