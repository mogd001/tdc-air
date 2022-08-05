library(tidyverse)
library(lubridate)
library(plotly)

merge_dat_files <- function(files) {
  # Merge a list of .dat files with equal structure.
  merge_df <- NULL
  
  for (f in files) {
    df <- read.table(f, head = FALSE, skip = 5) 
    names(df) <- df[1,]
    df <- df[-1,]
    
    merge_df <- rbind(merge_df, df)
  }
  
  rownames(merge_df) <- 1:nrow(merge_df)
  merge_df %>% 
    distinct() %>% 
    mutate(Date = mdy(Date),
           Time = hm(Time), 
           datetime = Date + Time,
           Time = format(datetime, format = "%H:%M:%S")) %>% 
    arrange(datetime)
}

# Short and Long files. Short, is if the filename contains "short".
directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(024) 5028i Download 02-08-2022"
filenames <- list.files(directory, pattern="*.dat", full.names=TRUE)

short_filenames <- filenames[grepl("short", tolower(filenames))]
long_filenames <- filenames[!grepl("short", tolower(filenames))]

day <- format(Sys.time(), "%Y%m%d")

# save to csv
short <- merge_dat_files(short_filenames)
short %>% 
  write.csv(paste0("outputs/", day, "_short_5028i.csv"))
# only output selected columns

long <- merge_dat_files(long_filenames) 
long %>% 
  write.csv(paste0("outputs/", day, "_long_5028i.csv"))


# visualise for testing
short <- tibble(short) %>%
  mutate(pm = as.numeric(pm))

p_short<- ggplot(long, aes(Date, pm)) + 
  geom_point(size = 1, color = "red") +
  scale_y_continuous(limits = c(0, NA))

long <- tibble(long) %>% 
  mutate(pm = as.numeric(pm))

p_long <- ggplot(long, aes(Date, pm)) + 
  geom_point(size = 1, color = "red") +
  scale_y_continuous(limits = c(0, NA))

ggplotly(p_short)
ggplotly(p_long)
