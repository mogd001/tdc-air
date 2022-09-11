library(tdcR)
library(tidyverse)
library(lubridate)

get_aq_data <- function(site, measurement) {
  # Get air quality data from server.
  get_data_site_measurement(site = site, measurement = measurement, from = "Data start", to = "Data end") %>% 
    group_by(date) %>% 
    summarise(
      value = max(value),
    ) %>% 
    filter(value > 0)
}
 
measurements <- get_measurements(site = site)


##### IMPORT AND TIDY
# - air quality data (PM2.5 and PM10)
# - metereological data (wind, temperature, rainfall)


###### AQ Richmond Central at Plunket [aqr]###### 
site <- "AQ Richmond Central at Plunket"

aqr_pm2p5_day <- get_aq_data(site = site, measurement = "PM2.5 (24 Hour)") %>% 
  mutate(measurement = "PM2.5", site = site)
aqr_pm10_day <- get_aq_data(site = site, measurement = "PM10 (24 Hour)") %>% 
  mutate(measurement = "PM10", site = site)

aqr_day <- bind_rows(aqr_pm2p5_day, aqr_pm10_day)


aqr_pm2p5_annual <- get_aq_data(site = site, measurement = "PM2.5 (annual)") %>% 
  mutate(measurement = "PM2.5", year = year(date), site = site) %>% 
  select(year, measurement, value, site)
aqr_pm10_annual <- get_aq_data(site = site, measurement = "PM10 (annual)") %>%
  mutate(measurement = "PM10", year = year(date), site = site) %>% 
  select(year, measurement, value, site)

aq_annual <- bind_rows(aqr_pm2p5_annual, aqr_pm10_annual)


###### AQ Motueka at Goodman Park [AQM] ######
site <- "AQ Motueka at Goodman Park"

aqm_day <- get_aq_data(site = site, measurement = "PM2.5 (24hr) [PM2.5 (24hr) TDC Partisol]") %>% 
  mutate(measurement = "PM2.5", site = site) 


###### AQ Brightwater at Brightwater North [AQB] ######
site <- "AQ Brightwater at Brightwater North"

aqb_day <- get_aq_data(site = site, measurement = "PM2.5 (24hr) [PM2.5 (24hr) TDC Partisol]") %>% 
  mutate(measurement = "PM2.5", site = site) 


###### All Sites ######
aq_day <- bind_rows(aqr_day, aqm_day, aqb_day)
rm(aqr_pm2p5_day, aqr_pm10_day, aqr_pm2p5_annual, aqr_pm10_annual, aqm_day, aqb_day)


##### VISUALISE
aq_day
aq_annual


ggplot(aq_day, aes(date, value)) +
  geom_point() +
  facet_grid(measurement ~ site, scales="free_y")

ggplot(aq_annual, aes(year, value)) +
  geom_point() +
  facet_grid(measurement ~ site, scales="free_y")