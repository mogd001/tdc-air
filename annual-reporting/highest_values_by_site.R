library(tidyverse)
aq_day <- readRDS("data/aq_day.rds")

summary <- aq_day %>%  
  filter(reporting_period == 1) %>%
  group_by(site, measurement) %>% 
  slice_max(value, n = 5) %>% 
  select(date, measurement, value)