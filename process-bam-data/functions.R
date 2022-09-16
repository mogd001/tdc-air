merge_dat_files <- function(files) {
  # Merge a list of .dat files with equal structure.
  
  merge_df <- NULL
  
  for (f in files) {
    df <- read.table(f, head = FALSE, skip = 5)
    names(df) <- df[1, ]
    df <- df[-1, ]
    
    merge_df <- rbind(merge_df, df)
  }
  
  rownames(merge_df) <- 1:nrow(merge_df)
  merge_df %>%
    distinct() %>%
    mutate(
      Date = mdy(Date),
      Time = hm(Time),
      datetime = force_tz(Date + Time, tz = "Etc/GMT+12"),
      time = format(datetime, format = "%H:%M:%S")
    ) %>%
    arrange(datetime) %>%
    tibble()
}


clean_long_5min_data <- function(df) {
  # Clean up long 5 minute data (adding gaps and 0's).
  
  gaps <- df %>% filter(err == 1 & err_state_change == 1)
  
  gap_start <- gaps %>%
    select(datetime, err_type, dur) %>%
    transmute(
      datetime = datetime - minutes(5) + seconds(1),
      pm = 0
    )
  
  gap_end <- gaps %>%
    select(datetime, dur) %>%
    filter(dur > 1) %>%
    mutate(
      datetime = datetime + (dur - 1) * minutes(5),
      pm = 0
    ) %>%
    select(-dur)
  
  df_cleaned <- df %>%
    filter(!(err == 1 & err_state_change != 1)) %>% # filter error runs excluding start which become "gaps"
    mutate(
      pm = ifelse(err == 1, NA, pm)
    ) %>%
    select(datetime, pm) %>%
    bind_rows(list(gap_start, gap_end)) %>%
    arrange(datetime)
  
  list(df_cleaned, gaps)
}


# generate comments from gaps
generate_long_5min_comment <- function(gap) {
  # Generate comments for errors.
  base_string <- "Missing records for duration {duration} from {start_datetime} to {end_datetime} due to {reason}."
  
  gap %>%
    mutate(
      start_datetime = datetime,
      duration = dur * minutes(5), # convert to hours
      end_datetime = datetime + dur * minutes(5),
      comment = ifelse(err_type == 1, glue(base_string, reason = "due to value less than -20"),
                       ifelse(err_type == 2 & duration > minutes(30), glue(base_string, reason = "Watercare undertaking instrument audit"),
                              ifelse(err_type == 2, glue(base_string, reason = "due to value less than -50"),
                                     ifelse(err_type == 3, glue(base_string, reason = "Watercare undertaking instrument audit (power off)"),
                                            "Other error type, please inspect further."
                                     )
                              )
                       )
      )
    )
}


clean_long_daily_data <- function(df, df_5min_cleaned, gaps) {
  # Clean up long daily data (adding gaps and 0's).
  # Gaps identified in 5 minute dataset.

  # NOTE NEED TO SORT IF DURATION OF GAP CROSSES MULTIPLE DAYS
  re_calc_daily <- df_5min_cleaned %>% 
    filter(pm != 0) %>% 
    mutate(date = as.Date(datetime, tz = "Etc/GMT+12")) %>% 
    group_by(date) %>%
    summarise(pm_re_calc = mean(pm, na.rm = TRUE),
    ) 
  
  df <- df %>% 
    left_join(re_calc_daily, by = "date") %>% 
    left_join(gaps, by = "date") %>% 
    mutate(
      pm_out = if_else(is.na(dur), pm, if_else(dur < 12, pm_re_calc, NA_real_))
    ) %>% 
    select(date, pm_out)

  # Tidy up
  dates_na <- df %>% filter(is.na(pm_out))
  
  dates_na1 <- dates_na %>% 
    mutate(datetime = date + seconds(1), 
           pm_out = 0)
  dates_na2 <- dates_na %>% 
    mutate(datetime = date + days(1) + seconds(1), 
           pm_out = 0)
  
  df <- df %>% filter(!is.na(pm_out)) %>% 
    mutate(datetime = date + hours(23) + minutes(59) + seconds(59)) %>% 
    bind_rows(dates_na1, dates_na2) %>% 
    arrange(datetime) %>% 
    select(
      datetime,
      pm = pm_out
    )
}