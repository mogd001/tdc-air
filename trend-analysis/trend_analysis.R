library(tidyverse)
library(lubridate)
library(openair)
library(gridExtra)
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

site <- "AQ Richmond Central at Plunket"

aqr_pm2p5_day <- get_aq_data(site = site, measurement = "PM2.5 (24 Hour)") %>%
  mutate(measurement = "PM2.5", site = site)
aqr_pm10_day <- get_aq_data(site = site, measurement = "PM10 (24 Hour)") %>%
  mutate(measurement = "PM10", site = site)


#### PM10

# Check for null dates
aqr_pm10_day %>% filter(is.na(value))

# Openair-trend analysis
trend_data <- aqr_pm10_day %>%
  select(date, pm10 = value) %>%
  mutate(pm10 = round(pm10, 1)) %>% 
  drop_na()


# Data check
ggplot(trend_data, aes(date, pm10)) + 
  geom_col()

trend_data %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(prop_valid_days = n()/365)


trend_data$date <- lubridate::ymd_hms(paste(trend_data$date, "00:00:00"))

output <- TheilSen(
  selectByDate(trend_data, year = 2013:2022),
  pollutant = "pm10",
  xlab = "Year",
  ylab = expression(Richmond ~ Central ~ PM[10] ~ (mu * g / m^{
    3
  })),
  deseason = TRUE,
  slope.percent = FALSE,
  date.format = "%Y"
)

ggsave("outputs/aq_richmond_pm10_trend_10years-2013to2022.jpeg", plot = grid.arrange(output$plot), width = 10, height = 7)

#https://bookdown.org/david_carslaw/openair/theil-sen.html
output$data$res2


#### PM2.5

# Check for null dates
aqr_pm2p5_day %>% filter(is.na(value))

# Openair-trend analysis
trend_pm2p5_data <- aqr_pm2p5_day %>%
  select(date, pm2p5 = value) %>%
  mutate(pm2p5 = round(pm2p5, 1)) %>% 
  drop_na()

ggplot(trend_pm2p5_data, aes(date, pm2p5)) + 
  geom_col()

trend_pm2p5_data %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(prop_valid_days = n()/365)

trend_pm2p5_data$date <- lubridate::ymd_hms(paste(trend_pm2p5_data$date, "00:00:00"))

output2 <- TheilSen(
  selectByDate(trend_pm2p5_data, year = 2019:2022),
  pollutant = "pm2p5",
  xlab = "Year",
  ylab = expression(Richmond ~ Central ~ PM[2.5] ~ (mu * g / m^{
    3
  })),
  deseason = TRUE,
  slope.percent = FALSE,
  date.format = "%Y"
)

ggsave("outputs/aq_richmond_pm2p5_trend_10years-2019to2022.jpeg", plot = grid.arrange(output2$plot), width = 10, height = 7)

#https://bookdown.org/david_carslaw/openair/theil-sen.html
output2$data$res2

