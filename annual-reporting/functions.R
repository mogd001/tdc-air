library(tidyverse)
library(tdcR)
library(roxygen2)

get_aq_data <- function(site, 
                        measurement) {
  # Get air quality data from Hilltop Server.
  get_data_site_measurement(site = site, measurement = measurement, from = "Data start", to = "Data end") %>% 
    group_by(date) %>% 
    summarise(
      value = max(value),
    ) %>% 
    filter(value > 0)
}


get_rainfall_data <- function(site, 
                              from = from, 
                              to = to) {
  # Get rainfall data from Hilltop Server.
  get_data_site_measurement(site = site, measurement = "Rainfall", method = "Total", time_interval = NA, from = from, to = to, interval = "1 day", alignment = "00:00") %>% 
    rename(rainfall = value) %>%
    arrange(datetime) %>%
    mutate(
      rainfall = round(rainfall, digits = 2)
    ) %>%
    mutate(date = as.Date(datetime)) %>% 
    select(date, rainfall)
}


determine_meteo_variable <- function(meteo, 
                                     hr_start = 0, 
                                     hr_end = 24, 
                                     offset_day = 0, 
                                     allowable_missing_hrs = 1, 
                                     FUN = mean) {
  # Determines additional meteorological variables based on defined times of day.
  # hr_start : left bound of the hour filter.
  # hr_end : right bound of the hour filter.
  # offset_day : if required to return the average for the preceding (negative)/following days (positive).
  # allowable_missing_hrs : how many hours of the period can be missing.
  # func : function to use when calculating the variable. 

  meteo %>% 
    filter(hour >= hr_start & hour < hr_end) %>% 
    group_by(date) %>% 
    mutate(n_hrs_day = n()) %>% 
    ungroup() %>% 
    filter(n_hrs_day >= hr_end - hr_start - allowable_missing_hrs) %>% # apply allowable_missing_hrs
    group_by(date) %>% 
    summarise(value = FUN(value, na.rm = TRUE)) %>% 
    transmute(date, value) %>% 
    mutate(date = date + days(-offset_day))
}


get_meteorological_data <- function(site, 
                                    from = from, 
                                    to = to) {
  # Get wind and temperature data from Hilltop Server.
  
  ###### Wind
  wind_mps <- get_data_site_measurement(site = site, measurement = "Wind Speed (hourly)", method = "Average", time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00") %>% 
    arrange(datetime) %>%
    mutate(
      value = round(value/3.6, digits = 2), # convert to mps.
      hour = hour(datetime)
    ) %>%
    select(datetime, date, hour, value) 

  # 24-hour average (midnight to midnight)
  determine_meteo_variable(wind_mps, hr_start = 0, FUN = max)
  determine_meteo_variable(wind_mps, hr_start = 17)
  determine_meteo_variable(wind_mps, hr_start = 20)
  determine_meteo_variable(wind_mps, hr_start = 6, hr_end = 12)
  
  # 7-hour average (5 pm to midnight)
 
  # 4-hour average (8 pm to midnight)

  # 6-hour average (6 am to midday)

  # 6-hour average preceding day (6 pm to midnight)
  
  # Minimum 1-hour (Midnight to midnight)
  
  # Maximum 1-hour (Midnight to midnight)
  
  # Hourly average (Hour ending 5 pm, 1600)
  
  # Hourly average (Hour ending 8 pm, 1900)
  
  # No. hours (5 pm to midnight)
  # < 1 ms-1
  # < 2 ms-1
  # < 3 ms-1
  
  ###### Temperature
  temp <- get_data_site_measurement(site = site, measurement = "Air Temperature (continuous)", method = "Average", time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00") %>% 
    rename(air_temp = value) %>%
    arrange(datetime) %>%
    mutate(
      wind_speed = round(air_temp, digits = 2)
    ) %>%
    mutate(date = as.Date(datetime)) %>% 
    select(date, air_temp)
  
  
  # 24-hour average (midnight to midnight)
  
  # 7-hour average (5 pm to midnight)
  
  # 4-hour average (8 pm to midnight)
  
  # Minimum 1-hour (Midnight to midnight)
  
  # Minimum following day 1-hour (Midnight to midnight)
  
  # Max sample day less min day following 1-hour (Midnight to midnight)
  
  # Maximum 1-hour (Midnight to midnight)
  
  # Hourly average (Hour ending 5 pm)
  
  # Hourly average (Hour ending 8 pm)
  
  # No. hours (5 pm to midnight)
  # < 1 deg C
  # < 5 deg C
  # < 10 deg C
  
  
  
  # Wind direction
  # Hourly average (Hour ending 5 pm)
  
  # Hourly average (Hour ending 8 pm)
  
  wind %>% left_join(temp, by = "date")
}