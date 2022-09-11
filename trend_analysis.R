library(tidyverse)
library(lubridate)
library(openair)
library(Hilltop)

data <- HilltopData("C:/Users/matto/OneDrive - Tasman District Council/Desktop/Working/Hilltop/Riwaka_AQ.hts")

site <- "AQ Riwaka at Riwaka East"

aq_df <- GetData(data, site, "PM2.5 (1min) [PM2.5 (1min) Mote ES642]", "", "") %>%
  as.data.frame() %>%
  rename("pm2p5" = ".") %>%
  cbind(datetime = rownames(.)) %>%
  as.data.frame(row.names = 1:nrow(.)) %>%
  mutate(
    datetime = as.POSIXct(.$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+12"),
    site = site,
    date = as.Date(datetime, tz = "Etc/GMT+12"),
    hour = hour(datetime),
    date_hour = floor_date(datetime, "1 hour"),
    weekday = wday(datetime, week_start = 1)
  ) %>%
  drop_na() %>% 
  as_tibble()

summary(aq_df)

p <- ggplot(aq_df, aes(date, pm2p5)) +
  geom_point(size = 0.1) +
  labs(title = "AQ Riwaka East PM2.5", x = "Date", y = "PM2.5 (ug/m3)") 

#st <- smoothTrend(aq_df, pollutant = "pm2p5")
st <- smoothTrend(aq_df, pollutant = "pm2p5", statistic = "percentile",
            percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5))

st$data$data %>% 
  filter(variable == "percentile.50")
