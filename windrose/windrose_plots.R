library(openair)
library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

# https://bookdown.org/david_carslaw/openair/wind-roses.html
??windRose

# Load example data
wind_data <- read.csv("data/example_wind_data.csv") %>%
  as_tibble() %>%
  mutate(nzst = dmy_hm(nzst, tz = "NZ"), 
         date = as.Date(nzst, tz = "NZ"),
         date_hour = floor_date(nzst, "1 hour"))

# Single windrose plot
png(file=glue("single-windrose-plot.png"))  
windRose(wind_data, ws = "spd", wd = "dir",
         breaks = c(1, 5, 10, 15, 20, 25, 999),
         angle = 45,
         paddle = FALSE,
         width = 2.0,
         auto.text = FALSE,
         annotate = TRUE,
         grid.line = 5,
         key = list(labels = c("1-5","5-10","10-15","15-20","20-25",">25"), header="Windrose Plot ", footer="Wind Speed (km/h)",
                    plot.style = c("ticks", "border"), fit = "all", height = 1, space = "top"),
         par.settings = list(axis.line=list(col="lightgray")),
         col = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"))
dev.off()

# All windrose plots for specific date
target_date = as.Date("2021-06-03")
animation_data <- wind_data %>% 
  filter(date ==  target_date)

for (d in unique(animation_data$date_hour)){
  d <- as.POSIXct(d, origin = "1970-01-01 00:00:00", tz = "NZ")
  hr_wind_data = animation_data %>%
    filter(date_hour == d)
  
  fname <- gsub(":","", d)
  png(file=glue("plot-outputs/{fname}.png"))
  windRose(hr_wind_data, ws = "spd", wd = "dir",
           breaks = c(1,5,10,15, 20, 25, 999),
           angle = 45,
           paddle = FALSE,
           width = 2.0,
           auto.text = FALSE,
           annotate = TRUE,
           grid.line = 5,
           key = list(labels = c("1-5","5-10","10-15","15-20","20-25",">25"), header=paste("Windrose Plot ", d), footer="Wind Speed (km/h)",
                      plot.style = c("ticks", "border"),
                      fit = "all", height = 1, space = "top"),
           par.settings = list(axis.line=list(col="lightgray")),
           col = c("#012CFF", "#00D5F7", "#7CFD7F", "#FDE801", "#FF4503", "#7E0100"))
  dev.off()
}

# Test alternative windrose plot (polar plot) based on DataCamp exercise
wind <- wind_data

# Plot distribution of speed
summary(wind$spd)
boxplot(wind$spd)

outliers <- boxplot(wind$spd, plot=FALSE)$out
wind<-wind
wind<- wind[-which(wind$spd %in% outliers),]
boxplot(wind$spd)

library(plotly)
ggplotly(ggplot(wind, aes(x = spd)) + 
  geom_density() +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 0.3), expand = 0) +
  theme_classic())

# https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636/2
url <- 'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- read_html(url)
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions_raw %>% 
  set_names(~tolower(sub(' Direction', '', .x))) %>% 
  slice(-1) %>% 
  separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

directions

wind <- wind %>% 
  mutate(
    spd_bin = cut(spd, breaks=c(0, 2, 6, 8, 10)),
    dir_bin = cut(
      wd, 
      breaks = c(0, directions$degree_max, 360), 
      labels = c(directions$cardinal, 'N')),
    ws = spd,
    wd = dir
    ) %>%
  drop_na(spd_bin, dir_bin)

polarFreq(wind)
windRose(wind, paddle = FALSE, key = list(footer="Wind Speed (km/h)"))

ggplot(wind, aes(dir_bin, fill = spd_bin)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16)
