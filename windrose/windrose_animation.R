library(openair)
library(tidyverse)
library(lubridate)
library(animation)
library(ggpubr)

wind_data <- read.csv("data/example_wind_data.csv") %>%
  as_tibble() %>%
  mutate(nzst = dmy_hm(nzst, tz = "NZ"), 
         date = as.Date(nzst, tz = "NZ"),
         date_hour = floor_date(nzst, "1 hour"))

des <- c("Example Windrose Animation")

target_date = as.Date("2021-06-04")
wind_data <- wind_data %>% 
  filter(date ==  target_date)

# HTML Export
saveHTML(
  {
    for (d in unique(wind_data$date_hour)) {
      d <- as.POSIXct(d, origin = "1970-01-01 00:00:00", tz = "NZ")
      hr_wind_data = wind_data %>%
        filter(date_hour == d)
      
      p1 <- windRose(hr_wind_data,
                    ws = "spd", wd = "dir",
                    angle = 15,
                    cols = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"),
                    grid.line = list(20, lyt = 10, color = "black"),
                    paddle = FALSE,
                    auto.text = FALSE,
                    breaks = c(1, 5, 10, 15, 20, 25, 999),
                    offset = 5,
                    max.freq = 30, 
                    annotate = FALSE,
                    key = list(
                      labels = c("1-5", "5-10", "10-15", "15-20", "20-25", ">25"), header = paste0("SiteX Windrose ", d), footer = "Wind Speed (km/h)",
                      plot.style = c("ticks", "border"), fit = "all", height = 1, space = "bottom"
                    ),
                    par.settings = list(axis.line = list(col = "black")),
      )
      ani.pause()
    }
  },
  img.name = "wr",
  imgdir = "animation_outputs",
  htmlfile = "windrose_animation.html",
  autobrowse = FALSE,
  title = "Demo of Windrose Animation",
  description = des
)

# GIF Export
saveGIF(
  {
    for (d in unique(wind_data$date_hour)) {
      d <- as.POSIXct(d, origin = "1970-01-01 00:00:00", tz = "NZ")
      hr_wind_data = wind_data %>%
        filter(date_hour == d)
      
      p <- windRose(hr_wind_data,
                    ws = "spd", wd = "dir",
                    angle = 15,
                    cols = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"),
                    grid.line = list(20, lyt = 10, color = "black"),
                    paddle = FALSE,
                    auto.text = FALSE,
                    breaks = c(1, 5, 10, 15, 20, 25, 999),
                    offset = 5,
                    max.freq = 30, 
                    annotate = FALSE,
                    key = list(
                      labels = c("1-5", "5-10", "10-15", "15-20", "20-25", ">25"), header = paste0("SiteX Windrose ", d), footer = "Wind Speed (km/h)",
                      plot.style = c("ticks", "border"), fit = "all", height = 1, space = "bottom"
                    ),
                    par.settings = list(axis.line = list(col = "black")),
      )
      ani.pause()
    }
  },
  movie.name = "windrose_animation.gif",
  img.name = "wr",
  title = "Demo of a windrose animation"
)