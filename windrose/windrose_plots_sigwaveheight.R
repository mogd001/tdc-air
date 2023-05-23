library(openair)
library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

f1 <- "data/TASCAM2_waves@0.25.CSV" # "data/OOA Mohua_waves@0.25.CSV"

df <- read_csv(f1) %>% 
  transmute(speed = sig_wave_height_m, direction = mean_wave_direction, datetime = dmy_hm(datetime_nzst, tz = "Etc/GMT-12"))

site <- "tasman_bay" # godlen_bay


from <- min(df$datetime)
to <- max(df$datetime)

computed <- now()
breaks <- c(-999, 0.25, 0.5 ,0.75, 1.0, 1.5, 2.0, 2.5, 3.0, 999) # breaks between wave heights
temp <- embed(breaks, 2)[, 2:1]
labels <- c(paste0("wh ", temp[, 1], " - ", temp[, 2])) %>%
  as.character() 

col_names <- c()
for (i in 1:(length(breaks) - 1)) {
  x <- paste0("Interval", i)
  col_names <- append(col_names, x)
}

summary <- tibble()

wrp <- windRose(df,
                ws = "speed", wd = "direction",
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
                  labels = labels, header = paste0(site, " ", from, " to ", to, " computed at: ", computed), footer = "Significant Wave Height (m)",
                  plot.style = c("ticks", "border"), fit = "all", height = 1, space = "top"
                ),
                par.settings = list(axis.line = list(col = "lightgray"))
)

png(
  file = glue("outputs/{site}-single-windrose-plot.png"),
  width = 1500,
  height = 1500,
  res = 150
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
    start_time = format(start_time,  "%d/%m/%Y %H:%M:%S"),
    end_time = format(end_time,  "%d/%m/%Y %H:%M:%S")
  ) %>% 
  write_csv(glue("outputs/{site}_windrose_summary.csv"))