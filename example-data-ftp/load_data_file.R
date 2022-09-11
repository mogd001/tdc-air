library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

data <- read_delim("example-data-ftp/TDC_Unit1_PM25.dat") %>% 
  mutate(NZST_Datetime = ymd_hms(NZST_Datetime), 
         NZDT_Datetime  = ymd_hms(NZDT_Datetime))

ggplot(data, aes(NZST_Datetime, conc)) + 
  geom_area(color = "red", fill = "red", alpha = 0.2) + 
  theme_classic() + 
  labs(x = "Datetime (NZST)", y = "PM2.5 (ug/m3)") +
  scale_x_datetime(breaks = pretty_breaks()) + 
  scale_y_continuous(expand = c(0, 0))