library(openair)
library(tidyverse)
library(lubridate)
library(glue)
library(rvest)
library(readxl)

f1 <- "data/Ohakea Aero - 2002 - 2007 hourly wind speed & direction.xlsx"
f2 <- "data/Ohakea Aero AWS - 2002 - 2012 hourly wind speed & direction.xlsx"

ohakea_aero <- read_excel(f1) %>% 
  transmute(wind_speed = speed_mps, wind_direction = dir_deg, datetime = date_nzst)
ohakea_aero_aws <- read_excel(f2) %>% 
  transmute(wind_speed = speed_mps, wind_direction = dir_deg, datetime = date_nzst)

site <- "ohakea_aero_aws"
df <- ohakea_aero_aws

from <- min(df$datetime)
to <- max(df$datetime)

computed <- now()
breaks <- c(-999, 3, 5.6, 7, 8.7, 11.3, 14.3, 17.4, 20.6, 999) # breaks between wind speeds
temp <- embed(breaks, 2)[, 2:1]
labels <- c(paste0("ws ", temp[, 1], " - ", temp[, 2])) %>%
  as.character() 

col_names <- c()
for (i in 1:(length(breaks) - 1)) {
  x <- paste0("Interval", i)
  col_names <- append(col_names, x)
}

summary <- tibble()

wrp <- windRose(df,
  ws = "wind_speed", wd = "wind_direction",
  #type = "month",  # break down plot by time period, see cutData for potential ways of cutting the data
  breaks = breaks,
  angle = 22.5,
  paddle = FALSE,
  offset = 15,
  width = 2.0,
  auto.text = FALSE,
  annotate = TRUE,
  grid.line = 5,
  key = list(
    labels = labels, header = paste0(site, " ", from, " to ", to, " computed at: ", computed), footer = "Wind Speed (m/s)",
    plot.style = c("ticks", "border"), fit = "all", height = 1, space = "top"
  ),
  par.settings = list(axis.line = list(col = "lightgray"))
)

png(
  file = glue("outputs/{site}-single-windrose-plot.png"),
  width = 750,
  height = 750,
  res = 72
)
print(wrp$plot)
dev.off()

out_df <- wrp$data %>%
  ungroup() %>%
  select(c("wd", col_names)) %>%
  setNames(c("wd", labels)) %>%
  mutate()

wd <- out_df$wd
out_df <- out_df %>%
  select(-wd)

out_df <- cbind(out_df[1], out_df[3:length(breaks) - 1] - out_df[2:length(breaks) - 2]) %>%
  mutate(wd = wd) %>%
  relocate(wd) %>% 
  filter(!(wd < 0)) # remove -999

write_csv(out_df, glue("outputs/{site}-windrose-plot-data.csv"))

summary <- summary %>% 
  bind_rows(tibble(site = site, start_time = min(df$datetime, na.rm = TRUE), end_time = max(df$datetime, na.rm = TRUE)))

summary %>% 
  mutate(
    start_time = force_tz(start_time, tz = "UTC"),
    end_time = force_tz(end_time, tz = "UTC")
  ) %>% 
  write_csv(glue("outputs/windrose_summary_ohakea.csv"))