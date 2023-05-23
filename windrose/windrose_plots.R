library(openair)
library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

library(tdcR)

# https://bookdown.org/david_carslaw/openair/wind-roses.html
??windRose

from <- "Data start" #from <- "Data start"
to <- "Data end" #to <- "Data end"
computed <- now()
breaks <- c(-999, 3, 5.6, 7, 8.7, 11.3, 14.3, 17.4, 20.6, 999) # breaks between wind speeds

temp <- embed(breaks, 2)[, 2:1]
labels <- c(paste0("ws ", temp[, 1], " - ", temp[, 2])) %>%
  as.character() 

# update first and last labels manually
labels[1] <- paste0("ws <=", breaks[2]) 
labels[length(labels)] <- paste0("ws >", breaks[length(breaks)-1])

col_names <- c()
for (i in 1:(length(breaks) - 1)) {
  x <- paste0("Interval", i)
  col_names <- append(col_names, x)
}

summary <- tibble()

sites <- c("HY Tasman Bay Buoy", 
           "HY Golden Bay Buoy", 
           "HY Takaka at Aerodrome",
           "HY Richmond Weather at Race Course")

for (site in sites) {
  print(site)
  if (site == "HY Golden Bay Buoy") {
    ws_var <- "Wind Speed (6m) [Wind Speed (6m)]"
    wd_var <- "Wind Direction (6m) [Wind Direction (6m)]"
  } else if (site %in% c("HY Richmond Weather at Race Course", "HY Motueka at Sportspark")) {
    ws_var <- "Wind Speed (hourly)"
    wd_var <- "Wind Direction (hourly)"
  } else {
    ws_var <- "Wind Speed"
    wd_var <- "Wind Direction"
  }

  wind_speed <- get_data_site_measurement(
    site = site,
    measurement = ws_var,
    from = from,
    to = to
  )

  wind_direction <- get_data_site_measurement(
    site = site,
    measurement = wd_var,
    from = from,
    to = to
  )

  wind_data <- wind_speed %>%
    transmute(datetime, wind_speed = value) %>%
    left_join(select(wind_direction, c(datetime, value)), by = "datetime") %>%
    rename(wind_direction = value) %>%
    mutate(
      wind_speed = wind_speed / 3.6, # convert km/h to m/s
      date = as.Date(datetime, tz = "Etc/GMT-12")
    )

  wrp <- windRose(wind_data,
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
    bind_rows(tibble(site = site, start_time = min(wind_data$datetime), end_time = max(wind_data$datetime)))
  
}

# Restricted sites 
library(zoo)
library(broom)
library(Hilltop)

restricted_sites <- c("HY Farewell Spit (AWS)", "HY Seperation Point (AWS)")

for (site in restricted_sites) {
  
  data <- HilltopData("M:/Datafiles/Archive/Restricted Access Data.hts") 
  wind_speed <- GetData(data, site, "Wind Speed", "", "") %>% 
    tidy() %>% 
    mutate(datetime = force_tz(index, tz = "Etc/GMT-12")) %>% 
    select(datetime, value)
  wind_direction <- GetData(data, site, "Wind Direction", "", "") %>% 
    tidy() %>% 
    mutate(datetime = force_tz(index, tz = "Etc/GMT-12")) %>% 
    select(datetime, value)

  disconnect(data)
  
  wind_data <- wind_speed %>%
    transmute(datetime, wind_speed = value) %>%
    left_join(select(wind_direction, c(datetime, value)), by = "datetime") %>%
    rename(wind_direction = value) %>%
    mutate(
      wind_speed = wind_speed / 3.6, # convert km/h to m/s
      date = as.Date(datetime, tz = "Etc/GMT-12")
    )
  
  wrp <- windRose(wind_data,
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
    relocate(wd)
  write_csv(out_df, glue("outputs/{site}-windrose-plot-data.csv"))
  
  summary <- summary %>% 
    bind_rows(tibble(site = site, start_time = min(wind_data$datetime), end_time = max(wind_data$datetime)))
  
}

summary %>% 
  mutate(
    start_time = force_tz(start_time, tz = "UTC"),
    end_time = force_tz(end_time, tz = "UTC")
  ) %>% 
write_csv(glue("outputs/windrose_summary.csv"))