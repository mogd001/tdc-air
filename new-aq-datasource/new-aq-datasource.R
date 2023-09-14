library(tdcR)
library(tidyverse)
library(lubridate)

###### AQ Richmond Central at Plunket [aqr] ######
### This script transforms historic air quality data into the new air quality dataset

custom_round <- function(x) {
  # function to perform rounding such than values that are 0.5 are rounded upwards (not R's default rounding!).
  dec_x <- x - floor(x)
  ifelse(dec_x >= 0.5, ceiling(x), floor(x))
}


transform_to_new_data_source <- function(aq_df) {
  # aq_df is the original air quality data
  # increment by 1 second then remove any datetimes with non-zero minutes and seconds
  aq_df %>%  
    mutate(datetime = datetime + seconds(1)) %>% 
    filter(minute(datetime) == 0, second(datetime) == 0) %>% 
    mutate(datetime = force_tz(datetime, "UTC"), # prepare for input into hilltop 
           date = as.character(datetime, format = "%Y%m%d"),
           time = as.character(datetime, format = "%H:%M:%S")) %>%  
    select(date, time, value)
} 


######

site <- "AQ Richmond Central at Plunket"
ep <- "http://envdata.tasman.govt.nz/data.hts?"
dev_ep <- "http://envdatadev.tasman.govt.nz/data.hts?"

ms <- get_measurements(dev_ep, site = site)

# 5028i BAM PM10 24 hr
aqr_pm10_daily <- get_data_site_measurement(dev_ep, site = site, measurement = "PM10 (24hr) [PM10 (24hr) TDC 5028i BAM]", from = "Data start", to = "Data end") %>% 
  transform_to_new_data_source() 
aqr_pm10_daily %>% 
  write_csv("outputs/pm10_24hr_upload.csv")

# 5028i BAM PM2.5 24 hr
aqr_pm2p5_daily <- get_data_site_measurement(dev_ep, site = site, measurement = "PM2.5 (24hr) [PM2.5 (24hr) TDC 5028i BAM]", from = "Data start", to = "Data end") %>% 
  transform_to_new_data_source() 
aqr_pm2p5_daily %>% 
  write_csv("outputs/pm2p5_24hr_upload.csv")

# FH62 BAM PM10 24 hr
aqr_pm10_fh62_daily <- get_data_site_measurement(dev_ep, site = site, measurement = "PM10 (24hr) Adjusted [PM10 (24hr) TDC FH62 BAM]", from = "Data start", to = "Data end") %>% 
  transform_to_new_data_source() #%>% 
  #mutate(value = custom_round(value)) 

aqr_pm10_fh62_daily %>% 
  write_csv("outputs/pm10_24hr_FH62_upload.csv")

# FH62 BAM PM10 30 min
aqr_pm10_fh62_30min <- get_data_site_measurement(dev_ep, site = site, measurement = "PM10 (30min) [PM10 (30min) TDC FH62 BAM]", from = "Data start", to = "Data end") 

aqr_pm10_fh62_30min_cleaned <- aqr_pm10_fh62_30min %>%
  mutate(is_1sec = if_else(second(datetime) != 0, 1, 0), # identify all 1 second datetimes
         to_remove = if_else(lag(is_1sec, default = 0) == 1, 1, is_1sec)) %>%   # identify all dateimes immediately after the above 1 second datetime
  filter(to_remove != 1) %>% 
  mutate(datetime = force_tz(datetime, "UTC"), # prepare for input into hilltop 
         date = as.character(datetime, format = "%Y%m%d"),
         time = as.character(datetime, format = "%H:%M:%S")) %>% 
  select(date, time, value)

aqr_pm10_fh62_30min_cleaned %>% 
  write_csv("outputs/pm10_30min_FH62_upload.csv")