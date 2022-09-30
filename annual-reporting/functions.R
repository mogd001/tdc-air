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
  get_data_site_measurement(site = site, measurement = "Rainfall", method = "Total", from = from, to = to, interval = "1 day") %>% 
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
                                     FUN = mean, 
                                     filter_value = NA) {
  # Determines additional meteorological variables based on defined times of day.
  # hr_start : left bound of the hour filter.
  # hr_end : right bound of the hour filter.
  # offset_day : if required to return the average for the preceding (negative)/following days (positive).
  # allowable_missing_hrs : how many hours of the period can be missing.
  # FUN : function to use when calculating the variable. 
  # filter_value : additional variable if counting (through a sum) the number of hours the value is less than this filter_value.
  
  meteo <- meteo %>% 
    filter(hour >= hr_start & hour < hr_end) %>% 
    group_by(date) %>% 
    mutate(n_hrs_day = n()) %>% 
    ungroup() %>% 
    filter(n_hrs_day >= hr_end - hr_start - allowable_missing_hrs) # apply allowable_missing_hrs
    
  if (!is.na(filter_value)) {
    meteo <- meteo %>% 
      filter(value < filter_value) %>% 
      mutate(value = 1) # to be used with sum FUN.
  }
  
  meteo %>% 
    group_by(date) %>% 
    summarise(value = FUN(value, na.rm = TRUE)) %>% 
    transmute(date, value) %>% 
    mutate(date = date + days(-offset_day))
}


get_meteorological_data <- function(site, 
                                    from = from, 
                                    to = to) {
  # Get wind and temperature data from Hilltop Server.
  meteo_data <- tibble(date = seq(ymd(from), ymd(to), by = "1 day"))
  
  ##### Wind Speed
  wind_mps <- get_data_site_measurement(site = site, measurement = "Wind Speed (hourly)", 
                                        method = "Average", from = from, to = to, interval = "1 hour") %>% 
    arrange(datetime) %>%
    mutate(
      value = round(value/3.6, digits = 2), # convert to mps.
      hour = hour(datetime)
    ) %>%
    select(datetime, date, hour, value) 

  meteo_data <- meteo_data %>% 
    left_join(determine_meteo_variable(wind_mps) %>% rename(wind_mps_24h_avg = value), by = "date") %>%  # 24-hour average (midnight to midnight)
    left_join(determine_meteo_variable(wind_mps, hr_start = 17) %>% rename(wind_mps_7h_avg = value), by = "date") %>%  # 7-hour average (5 pm to midnight)
    left_join(determine_meteo_variable(wind_mps, hr_start = 20) %>% rename(wind_mps_4h_avg = value), by = "date") %>%   # 4-hour average (8 pm to midnight)
    left_join(determine_meteo_variable(wind_mps, hr_start = 6, hr_end = 12) %>% rename(wind_mps_6h_avg_am = value), by = "date") %>%  # 6-hour average (6 am to midday)
    left_join(determine_meteo_variable(wind_mps, hr_start = 18, offset_day = -1) %>% rename(wind_mps_6h_avg_pm_preceeding = value), by = "date") %>%  # 6-hour average preceding day (6 pm to midnight)
    left_join(determine_meteo_variable(wind_mps, FUN = min) %>% rename(wind_mps_min = value), by = "date") %>%  # Minimum 1-hour (Midnight to midnight)
    left_join(determine_meteo_variable(wind_mps, FUN = max) %>% rename(wind_mps_max = value), by = "date") %>%  # Maximum 1-hour (Midnight to midnight)
    left_join(determine_meteo_variable(wind_mps, hr_start = 16, hr_end = 17) %>% rename(wind_mps_1h_avg_16 = value), by = "date") %>%  # Hourly average (Hour ending 5 pm, start 1600)
    left_join(determine_meteo_variable(wind_mps, hr_start = 19, hr_end = 20) %>% rename(wind_mps_1h_avg_19 = value), by = "date") %>%  # Hourly average (Hour ending 8 pm, start 1900)
    left_join(determine_meteo_variable(wind_mps, hr_start = 17, FUN = sum, filter_value = 1) %>% rename(wind_mps_nhrs_less1 = value), by = "date") %>%  # No. hours (5 pm to midnight), < 1 ms-1
    left_join(determine_meteo_variable(wind_mps, hr_start = 17, FUN = sum, filter_value = 2) %>% rename(wind_mps_nhrs_less2 = value), by = "date") %>%  # No. hours (5 pm to midnight), < 2 ms-1
    left_join( determine_meteo_variable(wind_mps, hr_start = 17, FUN = sum, filter_value = 3) %>% rename(wind_mps_nhrs_less3 = value), by = "date") # No. hours (5 pm to midnight), < 3 ms-1
  
  #### Wind Direction
  wind_dir <- get_data_site_measurement(site = site, measurement = "Wind Direction (hourly)", 
                                        method = "Average", from = from, to = to, interval = "1 hour") %>% 
    arrange(datetime) %>%
    mutate(
      hour = hour(datetime)
    ) %>%
    select(datetime, date, hour, value) 
  
  meteo_data <- meteo_data %>% 
    left_join(determine_meteo_variable(wind_dir, hr_start = 16, hr_end = 17) %>% rename(wind_dir_1h_avg_16 = value), by = "date") %>%  # Hourly average (Hour ending 5 pm, start 1600)
    left_join(determine_meteo_variable(wind_dir, hr_start = 19, hr_end = 20) %>% rename(wind_dir_1h_avg_19 = value), by = "date") # Hourly average (Hour ending 8 pm, start 1900)

  ###### Temperature
  temp <- get_data_site_measurement(site = site, measurement = "Air Temperature (continuous)", 
                                    method = "Average", from = from, to = to, interval = "1 hour") %>% 
    arrange(datetime) %>%
    mutate(
      hour = hour(datetime)
    ) %>%
    select(datetime, date, hour, value) 
  
  meteo_data <- meteo_data %>% 
    left_join(determine_meteo_variable(temp) %>% rename(temp_degc_24h_avg = value), by = "date") %>%  # 24-hour average (midnight to midnight)
    left_join(determine_meteo_variable(temp, hr_start = 17) %>% rename(temp_degc_7h_avg = value), by = "date") %>% # 7-hour average (5 pm to midnight)
    left_join(determine_meteo_variable(temp, hr_start = 20) %>% rename(temp_degc_4h_avg = value), by = "date") %>%   # 4-hour average (8 pm to midnight)
    left_join(determine_meteo_variable(temp, FUN = min) %>% rename(temp_degc_min = value), by = "date") %>%  # Minimum 1-hour (Midnight to midnight)
    left_join(determine_meteo_variable(temp, FUN = max) %>% rename(temp_degc_max = value), by = "date") %>%  # Maximum 1-hour (Midnight to midnight)
    left_join(determine_meteo_variable(temp, offset_day = 1, FUN = min) %>% rename(temp_degc_1h_min_following = value), by = "date") %>% # Minimum following day 1-hour (Midnight to midnight)
    left_join(determine_meteo_variable(temp, hr_start = 16, hr_end = 17) %>% rename(temp_degc_1h_avg_16 = value), by = "date") %>%  # Hourly average (Hour ending 5 pm, start 1600)
    left_join(determine_meteo_variable(temp, hr_start = 19, hr_end = 20) %>% rename(temp_degc_1h_avg_19 = value), by = "date") %>%  # Hourly average (Hour ending 8 pm, start 1900)
    left_join(determine_meteo_variable(temp, hr_start = 17, FUN = sum, filter_value = 1) %>% rename(temp_degc_nhrs_less1 = value), by = "date") %>%  # No. hours (5 pm to midnight), < 1 deg C
    left_join(determine_meteo_variable(temp, hr_start = 17, FUN = sum, filter_value = 5) %>% rename(temp_degc_nhrs_less5 = value), by = "date") %>%  # No. hours (5 pm to midnight), < 5 deg C
    left_join(determine_meteo_variable(temp, hr_start = 17, FUN = sum, filter_value = 10) %>% rename(temp_degc_nhrs_less10 = value), by = "date") %>%  # No. hours (5 pm to midnight), < 10 deg C
    mutate(temp_degc_max_less_min_following = temp_degc_max - temp_degc_1h_min_following) # Max sample day less min day following 1-hour (Midnight to midnight)
  
  meteo_data
}