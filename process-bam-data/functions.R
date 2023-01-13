library(RODBC)
library(DBI)

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
  merge_df <- merge_df %>%
    distinct() %>%
    mutate(
      Date = mdy(Date),
      Time = hm(Time),
      datetime = force_tz(Date + Time, tz = "Etc/GMT-12"),
      time = format(datetime, format = "%H:%M:%S")
    ) %>%
    arrange(datetime) %>%
    tibble()

  return(merge_df)
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
      pm = ifelse(err == 1, NA, round(pm, 1))
    ) %>%
    select(datetime, pm) %>%
    bind_rows(list(gap_start, gap_end)) %>%
    arrange(datetime)

  return(list(df_cleaned, gaps))
}


generate_long_5min_comments <- function(gaps) {
  # Generate comments for errors (5 minute).
  base_string <- "Missing record for {duration} minutes from {format(start_datetime, '%d/%m/%Y %H%M')} to {format(end_datetime, '%d/%m/%Y %H%M')} due to {reason}."

  comments <- gaps %>%
    mutate(
      start_datetime = datetime,
      duration = dur * 5, # convert to minutes
      end_datetime = datetime + dur * minutes(5),
      comment = ifelse(err_type == 1, glue(base_string, reason = "value less than -20"),
        ifelse(err_type == 2 & duration > minutes(30), glue(base_string, reason = "Watercare undertaking instrument audit"),
          ifelse(err_type == 2, glue(base_string, reason = "value less than -50"),
            ifelse(err_type == 3, glue(base_string, reason = "Watercare undertaking instrument audit (power off)"),
              "Other error type, please inspect further."
            )
          )
        )
      )
    )

  return(comments)
}


clean_long_daily_data <- function(df, df_5min_cleaned, gaps) {
  # Clean up long daily data (adding gaps and 0's).
  # Gaps identified in 5 minute dataset.

  # NOTE: NEED TO CONSIDER IF DURATION OF GAP CROSSES MULTIPLE DAYS.
  re_calc_daily <- df_5min_cleaned %>%
    filter(pm != 0) %>%
    mutate(date = as.Date(datetime, tz = "Etc/GMT-12")) %>%
    group_by(date) %>%
    summarise(pm_re_calc = mean(pm, na.rm = TRUE))

  df <- df %>%
    left_join(re_calc_daily, by = "date") %>%
    left_join(gaps, by = "date") %>%
    mutate(
      pm_out = if_else(is.na(dur), pm, if_else(dur < 12, pm_re_calc, NA_real_))
    ) %>%
    select(date, pm, pm_out)

  gaps <- gaps %>%
    left_join(df %>% filter(pm != pm_out), by = "date")

  # Tidy up
  dates_na <- df %>% filter(is.na(pm_out))

  dates_na1 <- dates_na %>%
    mutate(
      datetime = date + seconds(1),
      pm_out = 0
    )

  # add gaps
  dates_na1_gaps <- dates_na %>%
    mutate(
      datetime = date + seconds(02),
      pm = NA,
      pm_out = NA
    )

  dates_na2 <- dates_na %>%
    mutate(
      datetime = date + days(1) + seconds(1),
      pm_out = 0
    )

  df <- df %>%
    filter(!is.na(pm_out)) %>%
    mutate(datetime = date + hours(23) + minutes(59) + seconds(59)) %>%
    bind_rows(dates_na1, dates_na1_gaps, dates_na2) %>%
    arrange(datetime) %>%
    select(
      datetime,
      pm = pm_out
    ) %>%
    mutate(
      pm = round(pm, 3)
    )

  df$datetime <- force_tz(df$datetime, tz = "Etc/GMT-12")

  return(list(df, gaps))
}


generate_long_daily_comments <- function(gaps) {
  # Generate comments for errors (daily).
  gaps <- gaps %>%
    mutate(
      err_type = if_else(dur < 12, 1, 2) # 1 = Type A, 2 = Type B as per Processing Routine Logic
    )

  err_type_1_base_string <- "Daily value recalculated from valid 5 minute readings due to {duration} minutes missing data caused by instrument check.  23 hours of data is required to calculate a valid daily value.  The recalculation changed the daily value from {round(pm, 3)} to {round(pm_out, 3)}."
  err_type_2_base_string <- "Missing record on {format(date, '%d/%m/%Y')} for 1 day due to {duration_hours} hours of missing data while Watercare completed instrument audit."

  comments <- gaps %>%
    mutate(
      duration = dur * 5, # convert to minutes
      duration_hours = round(duration / 60, 2),
      comment = ifelse(err_type == 1, glue(err_type_1_base_string),
        ifelse(err_type == 2, glue(err_type_2_base_string), NA)
      )
    )

  return(comments)
}


insert_comments_to_envmon <- function(comments, measurement = c("5min_pm10", "5min_pm2p5", "daily_pm10", "daily_pm2p5")) {
  # Save comments to database.
  site_id <- 9856 # Richmond Central at Plunket
  logged_by <- "TDC\\matto"

  if (measurement == "5min_pm10") {
    measurement <- "PM10 (30min) [PM10 (30min) TDC 5028i BAM]"
  } else if (measurement == "5min_pm2p5") {
    measurement <- "PM2.5 (30min) [PM2.5 (30min) TDC 5028i BAM]"
  } else if (measurement == "daily_pm10") {
    measurement <- "PM10 (24hr) [PM10 (24hr) TDC 5028i BAM]"
  } else if (measurement == "daily_pm2p5") {
    measurement <- "PM2.5 (24hr) [PM2.5 (24hr) TDC 5028i BAM]"
  } else {
    stop("Invalid measurement type for this site.")
  }

  comments_to_insert <- comments %>%
    transmute(CommentDate = datetime, Comment = comment) %>%
    mutate(
      Measurement = measurement,
      SiteID = site_id,
      LoggedBy = logged_by,
      Link = NA
    ) %>%
    relocate(Comment, .after = Measurement)
  # Remove timezone
  comments_to_insert$CommentDate <- force_tz(comments_to_insert$CommentDate, tzone = "UTC")

  conn <- dbConnect(odbc::odbc(),
    Driver = "SQL Server",
    Server = "TSRVSQL14",
    Database = "ENVMON",
    Trusted_Connection = "yes"
  )

  # Only insert comment if it hasn't been entered before.
  dbBegin(conn)
  dbWriteTable(conn, "tbl_Comments", comments_to_insert, append = TRUE)
  dbCommit(conn)

  # close db connection
  dbDisconnect(conn)
}
