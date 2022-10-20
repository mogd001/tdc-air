library(tdcR)
library(tidyverse)
library(lubridate)

source("functions.R")

# site <- "AQ Richmond Central at Plunket"
# measurements <- get_measurements(site = site)

# Set reporting interval and site levels
reporting_interval <- interval(start = ymd("20210901"), end = ymd("20220831")) # 2022 Reporting
site_levels <- list("AQ Richmond Central at Plunket", "AQ Motueka at Goodman Park", "AQ Brightwater at Brightwater North")

###### IMPORT AND TIDY ######
# - air quality data (PM2.5 and PM10)
# - meteorological data (wind, temperature, rainfall)

site_meteorology_mapping <- tribble(
  ~aq_site, ~rainfall_site, ~wind_temp_site,
  "AQ Richmond Central at Plunket", "HY Richmond Weather at TDC Roof", "HY Richmond Weather at TDC Roof",
  "AQ Motueka at Goodman Park", "GW 2166 - Tui Close", "HY Motueka at Sportspark",
  "AQ Brightwater at Brightwater North", "HY Wairoa at Haycock Rd", "HY Richmond Weather at TDC Roof"
)

###### AQ Richmond Central at Plunket [aqr] ######
site <- "AQ Richmond Central at Plunket"

aqr_pm2p5_day <- get_aq_data(site = site, measurement = "PM2.5 (24 Hour)") %>%
  mutate(measurement = "PM2.5", site = site)
aqr_pm10_day <- get_aq_data(site = site, measurement = "PM10 (24 Hour)") %>%
  mutate(measurement = "PM10", site = site)

# load rainfall and meteorology data
from <- format(min(aqr_pm2p5_day$date, aqr_pm10_day$date), "%Y%m%d")
to <- format(max(aqr_pm2p5_day$date, aqr_pm10_day$date), "%Y%m%d")

site_rainfall <- filter(site_meteorology_mapping, aq_site == !!site)[["rainfall_site"]]

rainfall <- get_rainfall_data(site_rainfall, from, to)

site_meteo <- filter(site_meteorology_mapping, aq_site == !!site)[["wind_temp_site"]]
meteo <- get_meteorological_data(site_meteo, from, to)

base_dates <- tibble(date = seq(min(aqr_pm2p5_day$date), max(aqr_pm2p5_day$date), by = "days"))
aqr_pm2p5_day <- base_dates %>%
  left_join(aqr_pm2p5_day, by = "date") %>%
  left_join(rainfall, by = "date") %>%
  left_join(meteo, by = "date") %>%
  mutate_at(vars(site), ~ replace_na(., !!site)) %>%
  mutate_at(vars(measurement), ~ replace_na(., "PM2.5"))

base_dates <- tibble(date = seq(min(aqr_pm10_day$date), max(aqr_pm10_day$date), by = "days"))
aqr_pm10_day <- base_dates %>%
  left_join(aqr_pm10_day, by = "date") %>%
  left_join(rainfall, by = "date") %>%
  left_join(meteo, by = "date") %>%
  mutate_at(vars(site), ~ replace_na(., !!site)) %>%
  mutate_at(vars(measurement), ~ replace_na(., "PM10"))

aqr_day <- bind_rows(aqr_pm2p5_day, aqr_pm10_day)

aqr_pm2p5_annual <- get_aq_data(site = site, measurement = "PM2.5 (annual)") %>%
  mutate(measurement = "PM2.5", year = year(date), site = site) %>%
  select(year, measurement, value, site)
aqr_pm10_annual <- get_aq_data(site = site, measurement = "PM10 (annual)") %>%
  mutate(measurement = "PM10", year = year(date), site = site) %>%
  select(year, measurement, value, site)

aq_annual <- bind_rows(aqr_pm2p5_annual, aqr_pm10_annual)

###### AQ Motueka at Goodman Park [aqm] ######
site <- "AQ Motueka at Goodman Park"

aqm_day <- get_aq_data(site = site, measurement = "PM2.5 (24hr) [PM2.5 (24hr) TDC Partisol]") %>%
  mutate(measurement = "PM2.5", site = site)

# load rainfall and meteorology data
from <- format(min(aqm_day$date), "%Y%m%d")
to <- format(max(aqm_day$date), "%Y%m%d")

site_rainfall <- filter(site_meteorology_mapping, aq_site == !!site)[["rainfall_site"]]

rainfall <- get_rainfall_data(site_rainfall, from, to)

site_meteo <- filter(site_meteorology_mapping, aq_site == !!site)[["wind_temp_site"]]
meteo <- get_meteorological_data(site_meteo, from, to)

base_dates <- tibble(date = seq(min(aqm_day$date), max(aqm_day$date), by = "days"))
aqm_day <- base_dates %>%
  left_join(aqm_day, by = "date") %>%
  left_join(rainfall, by = "date") %>%
  left_join(meteo, by = "date") %>%
  mutate_at(vars(site), ~ replace_na(., !!site)) %>%
  mutate_at(vars(measurement), ~ replace_na(., "PM2.5"))

###### AQ Brightwater at Brightwater North [aqb] ######
site <- "AQ Brightwater at Brightwater North"

aqb_day <- get_aq_data(site = site, measurement = "PM2.5 (24hr) [PM2.5 (24hr) TDC Partisol]") %>%
  mutate(measurement = "PM2.5", site = site)

# load rainfall and meteorology data
from <- format(min(aqb_day$date), "%Y%m%d")
to <- format(max(aqb_day$date), "%Y%m%d")

site_rainfall <- filter(site_meteorology_mapping, aq_site == !!site)[["rainfall_site"]]

rainfall <- get_rainfall_data(site_rainfall, from, to)

site_meteo <- filter(site_meteorology_mapping, aq_site == !!site)[["wind_temp_site"]]
meteo <- get_meteorological_data(site_meteo, from, to)

base_dates <- tibble(date = seq(min(aqb_day$date), max(aqb_day$date), by = "days"))
aqb_day <- base_dates %>%
  left_join(aqb_day, by = "date") %>%
  left_join(rainfall, by = "date") %>%
  left_join(meteo, by = "date") %>%
  mutate_at(vars(site), ~ replace_na(., !!site)) %>%
  mutate_at(vars(measurement), ~ replace_na(., "PM2.5"))

###### All Sites ######
aq_day <- rbind(aqm_day, aqb_day, aqr_day) %>%
  mutate(
    site = factor(site, levels = site_levels, ordered = TRUE),
    reporting_period = if_else(date %within% reporting_interval, 1, 0)
  )

rm(aqr_pm2p5_day, aqr_pm10_day, aqr_pm2p5_annual, aqr_pm10_annual, aqm_day, aqb_day)

aq_day
aq_annual

# Add node and normalisation (Emily Wilton's, Environet study)
aq_day$node <- mapply(calculate_pollution_node, aq_day$wind_kph_24h_avg, aq_day$temp_degc_4h_avg)
aq_day$normalised_pm <- mapply(normalise_pm10, aq_day$measurement, aq_day$value, aq_day$node)

saveRDS(aq_day, file = "aq_day.RDS")
