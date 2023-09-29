library(tidyverse)
library(lubridate)
library(glue)
library(patchwork)
library(readxl)

# source("01_load_aq_data.R")
# saveRDS(aq_day, "aq_day.rds")
# source("02_modelling.R")

aq_day <- readRDS("data/aq_day.rds")

# Load exceedance data
exceedances <- read_excel("data/pm10_exceedance_data.xlsx") %>%
  mutate(
    normalised_median = round(normalised_median, 0),
    normalised_75th_percentile = round(normalised_75th_percentile, 0)
  )

aq_day_reporting_period <- filter(aq_day, reporting_period == 1) %>%
  mutate(
    year = year(date),
    month = as.numeric(month(date)),
    reporting_year = if_else(month > 8, year + 1, year)
  )

labels <- tribble(
  ~code, ~code_type, ~measurement, ~x, ~y, ~label,
  "daily_naqs_pm10", "daily", "PM10", min(aq_day_reporting_period$date), 50, National ~ Air ~ Quality ~ Standard ~ 24 ~ Hour ~ PM[10] ~ (50 ~ mu * g / m^{
    3
  }),
  "daily_who_pm10", "daily", "PM10", min(aq_day_reporting_period$date), 45, WHO ~ 2021 ~ Guideline ~ 24 ~ Hour ~ PM[10] ~ (45 ~ mu * g / m^{
    3
  }),
  "annual_who_pm10", "annual", "PM10", min(aq_day_reporting_period$date), 15, WHO ~ 2021 ~ Guideline ~ Annual ~ PM[10] ~ (15 ~ mu * g / m^{
    3
  }),
  # "daily_naqs_pm2p5", "daily", "PM2.5", min(aq_day_reporting_period$date), 25, Draft~National~Air~Quality~Standard~24-Hour~PM[2.5]~(25~mu*g/m^{3}),
  "daily_who_pm2p5", "daily", "PM2.5", min(aq_day_reporting_period$date), 15, WHO ~ 2021 ~ Guideline ~ 24 ~ Hour ~ PM[2.5] ~ (15 ~ mu * g / m^{
    3
  }),
  # "annual_naqs_pm2p5", "annual", "PM2.5", min(aq_day_reporting_period$date), 10, National~Air~Quality~Standard~24-Hour~PM[2.5]~(10~mu*g/m^{3}),
  "annual_who_pm2p5", "annual", "PM2.5", min(aq_day_reporting_period$date), 5, WHO ~ 2021 ~ Guideline ~ Annual ~ PM[2.5] ~ (5 ~ mu * g / m^{
    3
  })
)


labels_general <- tribble(
  ~x, ~y, ~label,
  ymd(20230601), 55, "Winter 2023"
)

###### All plots in one - not used in report
labels_daily <- labels %>% filter(code_type == "daily")

p_reporting_period <- ggplot() +
  geom_col(data = aq_day_reporting_period, aes(date, value, fill = measurement), size = 0.5) +
  labs(x = "", y = expression(PM ~ (mu * g / m^{
    3
  })), title = "PM Daily") +
  geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 45, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 15, color = "orange", linetype = "dashed") +
  geom_text(data = labels_daily, mapping = aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 2, color = "black", parse = TRUE) +
  scale_fill_manual(name = "Legend", values = c("PM10" = "red", "PM2.5" = "orange"), labels = c(PM10 = expression(paste(~ PM[10])), PM2.5 = expression(paste(PM[2.5])))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 55), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_grid(site ~ measurement)

ggsave("outputs/p1.jpeg", plot = p_reporting_period, width = 14, height = 10)

###### Richmond daily PM10 concentrations
site <- "AQ Richmond Central at Plunket"
aq_day_richmond_pm10_reporting_period <- filter(aq_day_reporting_period, site == !!site & measurement == "PM10")
labels_daily_richmond_pm10 <- labels %>% filter(code_type == "daily" & measurement == "PM10")

p_aq_day_richmond_pm10 <- ggplot() +
  annotate("rect", xmin = ymd(20230501), xmax = ymd(20230831), ymin = 0, ymax = 60, alpha = 0.1, fill = "black") + # add winter rectangle
  geom_label(data = labels_general, aes(x, y, label = label)) +
  geom_col(data = aq_day_richmond_pm10_reporting_period, aes(date, value, color = measurement), size = 0.2, color = "black") +
  labs(x = "", y = expression(PM[10] ~ (mu * g / m^{
    3
  })), title = "Daily Records Richmond Central at Plunket") +
  geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 45, color = "red", linetype = "dashed") +
  geom_text(data = labels_daily_richmond_pm10, mapping = aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 3.5, color = "red", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "black"), labels = c(PM10 = expression(paste(~ PM[10])))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none")

ggsave("outputs/aq_day_richmond_pm10.jpeg", plot = p_aq_day_richmond_pm10, width = 10, height = 7)

# Month summary
df_pm10 <- aq_day_richmond_pm10_reporting_period %>%
  group_by(year, month) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_50 = sum(value > 50, na.rm = TRUE),
    count_exceedance_45 = sum(value > 45, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

# Winter summary
df2_pm10 <- aq_day_richmond_pm10_reporting_period %>%
  filter(month %in% c(5, 6, 7, 8)) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_50 = sum(value > 50, na.rm = TRUE),
    count_exceedance_45 = sum(value > 45, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

# Not-winter summary
df3_pm10 <- aq_day_richmond_pm10_reporting_period %>%
  filter(!month %in% c(5, 6, 7, 8)) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_50 = sum(value > 50, na.rm = TRUE),
    count_exceedance_45 = sum(value > 45, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

# Annual summary
df4_pm10 <- aq_day_richmond_pm10_reporting_period %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_50 = sum(value > 50, na.rm = TRUE),
    count_exceedance_45 = sum(value > 45, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

summary_day_richmond_pm10_reporting_period <- as_tibble(cbind(nms = names(df_pm10), round(t(df_pm10), 0)))
colnames(summary_day_richmond_pm10_reporting_period) <- c("nms", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
summary_day_richmond_pm10_reporting_period
clipr::write_clip(summary_day_richmond_pm10_reporting_period)

annual_mean <- aq_day_richmond_pm10_reporting_period %>%
  group_by(reporting_year) %>%
  summarise(
    mean = mean(value, na.rm = TRUE)
  )


###### Richmond yearly exceedances
p_richmond_exceedance <- exceedances %>%
  ggplot(aes(year, n_exceedance_richmond)) +
  geom_col(color = "blue", fill = "blue", alpha = 0.6) +
  geom_text(aes(label = n_exceedance_richmond), color = "blue", size = 3, vjust = -0.4) +
  #  geom_point(aes(x = year, y = second_highest_value), size = 3, shape = 3, color = "black", alpha = 0.4, inherit.aes = FALSE) +
  geom_point(aes(x = year, y = second_highest_value), size = 2, shape = 1, color = "red", inherit.aes = FALSE) +
  geom_text(aes(x = year, y = second_highest_value, label = second_highest_value), size = 3, color = "red", inherit.aes = FALSE, vjust = -0.8) +
  labs(x = "", title = "Exceedances and Second Highest Daily Records Richmond Central at Plunket") +
  scale_x_continuous(breaks = seq(2000, 2023, 1), expand = c(0, 0.2)) +
  scale_y_continuous(
    name = "Number of exceedances",
    limits = c(0, 140),
    breaks = seq(0, 140, 10),
    sec.axis = sec_axis(~ . * 1, breaks = seq(0, 140, 10), name = expression(Second ~ highest ~ PM[10] ~ (mu * g / m^{
      3
    }))),
    expand = c(0, NA)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks.y.left = element_line(colour = "blue"),
    axis.title.y.left = element_text(colour = "blue"),
    axis.line.y.left = element_line(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.ticks.y.right = element_line(colour = "red"),
    axis.title.y.right = element_text(colour = "red", angle = 90),
    axis.line.y.right = element_line(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/richmond_exceedances_pm10.jpeg", plot = p_richmond_exceedance, width = 10, height = 5)


###### Richmond yearly normalised
coeff <- 5

exceedances2 <- exceedances %>% gather(key = "type", value = "normalised_value", normalised_median, normalised_75th_percentile)

p_richmond_normalised <- ggplot() +
  geom_col(data = exceedances, aes(x = year, n_high_pollution_days), color = "#023020", fill = "#023020", alpha = 0.6) +
  geom_text(data = exceedances, aes(x = year, 0, label = n_high_pollution_days), color = "#023020", size = 3, vjust = -0.4) +
  geom_point(data = exceedances2, aes(x = year, y = normalised_value / coeff, shape = type), color = "red", size = 2, inherit.aes = FALSE) +
  geom_text(data = exceedances2, aes(x = year, y = normalised_value / coeff, label = normalised_value), color = "red", size = 3, vjust = -1.0, inherit.aes = FALSE) +
  labs(x = "", shape = "", title = "Normalisation for Meteorological Conditions Richmond Central at Plunket") +
  scale_shape_manual(values = c(18, 15), labels = c("Normalised 75th Percentile", "Normalised Median")) +
  scale_x_continuous(breaks = seq(2000, 2023, 1), expand = c(0, 0.4)) +
  scale_y_continuous(
    name = "Number of high pollution potential days",
    limits = c(0, 16),
    breaks = seq(0, 16, 2),
    sec.axis = sec_axis(~ . * coeff, breaks = seq(0, 80, 10), name = expression(Normalised ~ ~ PM[10] ~ (mu * g / m^{
      3
    }))),
    expand = c(0, NA)
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.88, 0.89),
    legend.box.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks.y.left = element_line(colour = "#023020"),
    axis.title.y.left = element_text(colour = "#023020"),
    axis.line.y.left = element_line(color = "#023020"),
    axis.text.y.left = element_text(color = "#023020"),
    axis.ticks.y.right = element_line(colour = "red"),
    axis.title.y.right = element_text(colour = "red", angle = 90),
    axis.line.y.right = element_line(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/richmond_normalised_pm10.jpeg", plot = p_richmond_normalised, width = 10, height = 5)


###### Richmond daily PM2.5 concentrations
site <- "AQ Richmond Central at Plunket"
aq_day_richmond_pm2p5_reporting_period <- filter(aq_day_reporting_period, site == !!site & measurement == "PM2.5")
labels_daily_richmond_pm2p5 <- labels %>% filter(code_type == "daily" & measurement == "PM2.5")

p_aq_day_richmond_pm2p5 <- ggplot() +
  annotate("rect", xmin = ymd(20230501), xmax = ymd(20230831), ymin = 0, ymax = 60, alpha = 0.1, fill = "black") + # add winter rectangle
  geom_label(data = labels_general, aes(x, y, label = label)) +
  geom_col(data = aq_day_richmond_pm2p5_reporting_period, aes(date, value, color = measurement), size = 0.2, color = "black") +
  labs(x = "", y = expression(PM[2.5] ~ (mu * g / m^{
    3
  })), title = "Daily Records Richmond Central at Plunket") +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_text(data = labels_daily_richmond_pm2p5, mapping = aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 3.5, color = "red", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "black"), labels = c(PM10 = expression(paste(~ PM[2.5])))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none")

ggsave("outputs/aq_day_richmond_pm2p5.jpeg", plot = p_aq_day_richmond_pm2p5, width = 10, height = 7)

# Month summary
df_pm2p5 <- aq_day_richmond_pm2p5_reporting_period %>%
  group_by(year, month) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 15, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )
clipr::write_clip(df_pm2p5)

# Winter summary
df2_pm2p5 <- aq_day_richmond_pm2p5_reporting_period %>%
  filter(month %in% c(5, 6, 7, 8)) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 12, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

# Not-winter summary
df3_pm2p5 <- aq_day_richmond_pm2p5_reporting_period %>%
  filter(!month %in% c(5, 6, 7, 8)) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 15, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

# Annual summary
df4_pm2p5 <- aq_day_richmond_pm2p5_reporting_period %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 15, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

summary_day_richmond_pm2p5_reporting_period <- as_tibble(cbind(nms = names(df_pm2p5), round(t(df_pm2p5), 1)))
colnames(summary_day_richmond_pm2p5_reporting_period) <- c("nms", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

summary_day_richmond_pm2p5_reporting_period
clipr::write_clip(summary_day_richmond_pm2p5_reporting_period)

annual_mean <- aq_day_richmond_pm2p5_reporting_period %>%
  group_by(reporting_year) %>%
  summarise(
    mean = mean(value, na.rm = TRUE)
  )
annual_mean


###### Motueka daily PM2.5 concentrations
site <- "AQ Motueka at Goodman Park"
aq_day_motueka_pm2p5_reporting_period <- filter(aq_day_reporting_period, site == !!site & measurement == "PM2.5")
labels_daily_motueka_pm2p5 <- labels %>% filter(code_type == "daily" & measurement == "PM2.5")  %>% 
  mutate(x = ymd("2023-01-01"))

p_aq_day_motueka_pm2p5 <- ggplot() +
  annotate("rect", xmin = ymd(20230501), xmax = ymd(20230831), ymin = 0, ymax = 60, alpha = 0.1, fill = "black") + # add winter rectangle
  geom_label(data = labels_general, aes(x, y, label = label)) +
  geom_col(data = aq_day_motueka_pm2p5_reporting_period, aes(date, value, color = measurement), size = 0.2, color = "black") +
  labs(x = "", y = expression(PM[2.5] ~ (mu * g / m^{
    3
  })), title = "Daily Records Motueka at Ledger Goodman Park") +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_text(data = labels_daily_motueka_pm2p5, mapping = aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 3.5, color = "red", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "black"), labels = c(PM10 = expression(paste(~ PM[2.5])))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none")

ggsave("outputs/aq_day_moteuka_pm2p5.jpeg", plot = p_aq_day_motueka_pm2p5, width = 10, height = 7)

# Month summary
df_mot_pm2p5 <- aq_day_motueka_pm2p5_reporting_period %>%
  group_by(year, month) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 15, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )
clipr::write_clip(df_mot_pm2p5)

# Winter summary
df2_mot_pm2p5 <- aq_day_motueka_pm2p5_reporting_period %>%
  filter(month %in% c(5, 6, 7, 8)) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 15, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

# Not-winter summary
df3_mot_pm2p5 <- aq_day_motueka_pm2p5_reporting_period %>%
  filter(!month %in% c(5, 6, 7, 8)) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 15, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

# Annual summary
df4_mot_pm2p5 <- aq_day_motueka_pm2p5_reporting_period %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    count_exceedance_25 = sum(value > 25, na.rm = TRUE),
    count_exceedance_15 = sum(value > 15, na.rm = TRUE),
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

summary_day_motueka_pm2p5_reporting_period <- as_tibble(cbind(nms = names(df_mot_pm2p5), round(t(df_mot_pm2p5), 1)))
colnames(summary_day_motueka_pm2p5_reporting_period) <- c("nms", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
#colnames(summary_day_motueka_pm2p5_reporting_period) <- c("nms", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

summary_day_motueka_pm2p5_reporting_period
clipr::write_clip(summary_day_motueka_pm2p5_reporting_period)

annual_mean <- aq_day_motueka_pm2p5_reporting_period %>%
  group_by(reporting_year) %>%
  summarise(
    mean = mean(value, na.rm = TRUE)
  )
annual_mean


###### Motueka daily PM2.5 vs Richmond daily PM2.5
sites_comparison <- c("AQ Richmond Central at Plunket", "AQ Motueka at Goodman Park")
aq_daypm2p5_comparison_reporting_period <- filter(aq_day_reporting_period, site %in% sites_comparison & measurement == "PM2.5") %>% 
  mutate(winter = ifelse(month %in% c(5, 6, 7, 8), "May - August", "September - April"),
         site = ifelse(site == "AQ Motueka at Goodman Park", "Motueka at Ledger Goodman Park", "Richmond Central at Plunket"))

aq_daypm2p5_comparison_reporting_period$site <- factor(aq_daypm2p5_comparison_reporting_period$site, levels = c("Richmond Central at Plunket", "Motueka at Ledger Goodman Park"))


labels_daily_comparison_pm2p5 <- labels %>% filter(code_type == "daily" & measurement == "PM2.5") 

p_aq_day_comparison_pm2p5_ts <- ggplot() + 
  annotate("rect", xmin = ymd(20230501), xmax = ymd(20230831), ymin = 0, ymax = 50, alpha = 0.1, fill = "black") + # add winter rectangle
  geom_label(data = labels_general, aes(ymd("20230601)"), 45, label = label)) +
  geom_step(aq_daypm2p5_comparison_reporting_period, mapping = aes(date, value, color = site), alpha = 0.5) +
  geom_text(data = labels_daily_comparison_pm2p5, mapping = aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 3.5, color = "red", parse = TRUE) +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  labs(x = "", y = expression(PM[2.5] ~ (mu * g / m^{
    3
  })), color = "Site") +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 50, 5), limits = c(0, 50), expand = c(0,0)) +
  scale_color_manual(values = c("orange", "magenta")) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_blank(),
        legend.position = c(0.135, 0.84),
        legend.box.background = element_rect(colour = "black"))


p_aq_day_comparison_pm2p5_bp <- ggplot() + 
  geom_boxplot(aq_daypm2p5_comparison_reporting_period, mapping = aes(site, value, fill = winter, color = winter), alpha = 0.4, size = 0.2, width=0.2, outlier.shape = NA, position=position_dodge(width = 0.5)) +
  geom_jitter(aq_daypm2p5_comparison_reporting_period, mapping = aes(site, value, color = winter), width = 0.05, size = 1, shape = 3, alpha = 0.5) +
#  geom_text(data = labels_daily_comparison_pm2p5, mapping = aes("AQ Richmond Central at Plunket", y, label = label, vjust = -0.15), size = 3.5, color = "red", parse = TRUE) +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  labs(x = "", y = expression(PM[2.5] ~ (mu * g / m^{
    3
  })), color = "Month") +
  scale_y_continuous(breaks = seq(0, 50, 5), limits = c(0, 50), expand = c(0,0)) +
  scale_color_manual(values = c("red", "blue")) + 
  scale_fill_manual(values = c("red", "blue")) + 
  theme_bw()  +
  theme(legend.title = element_blank(),
        legend.position = c(0.915, 0.84),
        legend.box.background = element_rect(colour = "black")
      ) + 
  guides(fill= "none",
         color = guide_legend(override.aes = list(linetype = c(0, 0))))

p_aq_day_comparison_pm2p5 <- p_aq_day_comparison_pm2p5_ts / p_aq_day_comparison_pm2p5_bp +
  plot_annotation(
    title = "Comparison Richmond vs Motueka"
  )

ggsave("outputs/aq_day_comparison_pm2p5.jpeg", plot = p_aq_day_comparison_pm2p5, width = 10, height = 7)


###### Brightwater daily PM2.5 concentrations
site <- "AQ Brightwater at Brightwater North"
#aq_day_brightwater_pm2p5_reporting_period <- filter(aq_day_reporting_period, site == !!site & measurement == "PM2.5")
aq_day_brightwater_pm2p5_reporting_period <- filter(aq_day, site == !!site & measurement == "PM2.5")
labels_daily_brightwater_pm2p5 <- labels %>% filter(code_type == "daily" & measurement == "PM2.5") %>% 
  mutate(x = ymd("2023-08-01"))


my_dates <- function(d) {
  aq_day_brightwater_pm2p5_reporting_period %>% 
    subset(!is.na(value)) %>% 
    pull(date)
}

p_aq_day_brightwater_pm2p5 <- ggplot() +
  annotate("rect", xmin = ymd(20230620), xmax = ymd(20230831), ymin = 0, ymax = 60, alpha = 0.1, fill = "black") + # add winter rectangle
  geom_label(data = labels_general, aes(ymd(20230701), y, label = label)) +
  geom_col(data = aq_day_brightwater_pm2p5_reporting_period, aes(date, value, color = measurement), size = 0.2, color = "black") +
  labs(x = "", y = expression(PM[2.5] ~ (mu * g / m^{
    3
  })), title = "Daily Records Brightwater North 2023") +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed") +
  geom_text(data = labels_daily_brightwater_pm2p5, mapping = aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 3.5, color = "red", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "black"), labels = c(PM10 = expression(paste(~ PM[2.5])))) +
  
  scale_x_date(date_labels = "%d-%b", date_breaks = my_dates, expand = c(0, 0)) +
  
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none")

ggsave("outputs/aq_day_brightwater_pm2p5.jpeg", plot = p_aq_day_brightwater_pm2p5, width = 10, height = 7)

# ###### Richmond monthly daily PM10 distribution
# site <- "AQ Richmond Central at Plunket"
#
# aq_day_richmond_pm10_reporting_period <- aq_day_richmond_pm10_reporting_period %>%
#   mutate(date_month = format(date, "%Y-%b"))
# labels_daily_richmond_pm10 <- labels %>% filter(code_type == "daily" & measurement == "PM10")
#
# p_aq_day_richmond_pm10_monthly_boxplot <- ggplot() +
#   geom_boxplot(data = aq_day_richmond_pm10_reporting_period , aes(date_month, value), size = 0.5,  color = "red", fill = "red", alpha = 0.4) +
#   labs(x = "", y = expression(PM[10]~(mu*g/m^{3})), title = glue({site})) +
#   geom_hline(yintercept = 50, color = "black", linetype = "dashed") +
#   geom_text(data = labels_daily_richmond_pm10, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 4, color = "black", parse = TRUE) +
#   scale_color_manual(name = "Legend", values = c("PM10" = "red"), labels = c(PM10 = expression(paste(~PM[10])), PM2.5 = expression(paste(PM_2.5)))) +
#   scale_y_continuous(limits = c(0, max(aq_day_richmond_pm10$value, na.rm = TRUE)*1.02), expand = c(0, 0)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#
# ggsave("outputs/aq_day_richmond_pm10_distribution.jpeg", plot = p_aq_day_richmond_pm10, width = 10, height = 7)
#
#
# #########################
# p_aq_day_richmond_pm10_annual_boxplot <- ggplot() +
#   geom_boxplot(data = aq_day_richmond_pm10 , aes(year, value), size = 0.5,  color = "red", fill = "red", alpha = 0.4) +
#   labs(x = "", y = expression(PM[10]~(mu*g/m^{3})), title = glue({site})) +
#   geom_hline(yintercept = 50, color = "black", linetype = "dashed") +
#   geom_text(data = label1, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 4, color = "black", parse = TRUE) +
#   scale_color_manual(name = "Legend", values = c("PM10" = "red"), labels = c(PM10 = expression(paste(~PM[10])), PM2.5 = expression(paste(PM_2.5)))) +
#   scale_x_discrete() +
#   scale_y_continuous(limits = c(0, max(aq_day_richmond_pm10$value, na.rm = TRUE)*1.02), expand = c(0, 0)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#
# p_aq_day_richmond_pm10_annual_violin <- ggplot() +
#   geom_violin(data = aq_day_richmond_pm10 , aes(year, value), size = 0.5,  color = "red", fill = "red", alpha = 0.4) +
#   labs(x = "", y = expression(PM[10]~(mu*g/m^{3})), title = glue({site})) +
#   geom_hline(yintercept = 50, color = "black", linetype = "dashed") +
#   geom_text(data = label1, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 4, color = "black", parse = TRUE) +
#   scale_color_manual(name = "Legend", values = c("PM10" = "red"), labels = c(PM10 = expression(paste(~PM[10])), PM2.5 = expression(paste(PM_2.5)))) +
#   scale_x_discrete() +
#   scale_y_continuous(limits = c(0, max(aq_day_richmond_pm10$value, na.rm = TRUE)*1.02), expand = c(0, 0)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#
# p_aq_day_richmond_pm10_annual <- p_aq_day_richmond_pm10_annual_boxplot / p_aq_day_richmond_pm10_annual_violin
#
# ggsave("outputs/aq_annual_richmond_pm10.jpeg", plot = p_aq_day_richmond_pm10_annual, width = 10, height = 14)
#
