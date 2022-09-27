library(tidyverse)
library(tdcR)

get_aq_data <- function(site, measurement) {
  # Get air quality data from Hilltop server.
  get_data_site_measurement(site = site, measurement = measurement, from = "Data start", to = "Data end") %>% 
    group_by(date) %>% 
    summarise(
      value = max(value),
    ) %>% 
    filter(value > 0)
}


get_rainfall_data <- function(site, from = from, to = to){
  # Get rainfall data from Hilltop server.
  get_data_site_measurement(site = site, measurement = "Rainfall", method = "Total", time_interval = NA, from = from, to = to, interval = "1 day", alignment = "00:00") %>% 
    rename(rainfall_total = value) %>%
    arrange(datetime) %>%
    mutate(
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    mutate(date = as.Date(datetime)) %>% 
    select(date, rainfall_total)
}