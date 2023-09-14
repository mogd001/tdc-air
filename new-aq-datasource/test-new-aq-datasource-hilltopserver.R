library(tdcR)
library(tidyverse)
library(lubridate)

ep <- "http://envdata.tasman.govt.nz/data.hts?"
site <- "AQ Richmond Central at Plunket"

ms <- get_measurements(ep, site = site) %>% 
  filter(data_type == "AirQuality")

### Testing air quality new data source
data <- get_data_site_measurement(ep, site = site, measurement = "PM2.5 Yearly Average [PM2.5 5028i BAM (5 min)]", from = "Data start", to = "Data end")
