library(knitr)
library(tidyverse)
library(lubridate)
library(pROC)
library(precrec)
library(openair)
library(patchwork)
library(gridExtra)
library(cowplot)
library(zoo)

source("functions.R")

# source("01_load_aq_data.R")
# saveRDS(aq_day, file = "temp.RDS")
aq_day <- readRDS("data/aq_day.rds")

site <- "AQ Richmond Central at Plunket"

aq_day_richmond_pm10 <- filter(aq_day, site == !!site & measurement == "PM10") %>%
  mutate(
    year = year(date),
    month = as.numeric(month(date)),
    reporting_year = if_else(month > 8, year + 1, year)
  )

# Summary stats total
summary_stats <- group_by(aq_day_richmond_pm10, reporting_year) %>%
  summarise(
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    count_exceedance = sum(value > 50, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

clipr::write_clip(summary_stats)
kable(summary_stats)

# Openair-trend analysis
trend_data <- aq_day_richmond_pm10 %>%
  select(date, pm10 = value) %>%
  drop_na()

trend_data$date <- lubridate::ymd_hms(paste(trend_data$date, "00:00:00"))

output1a <- TheilSen(
  selectByDate(trend_data, year = 2006:reporting_year),
  pollutant = "pm10",
  xlab = "Year",
  ylab = expression(Richmond ~ Central ~ PM[10] ~ (mu * g / m^{
    3
  })),
  deseason = TRUE,
  slope.percent = FALSE
)

ggsave("outputs/aq_richmond_pm10_trend_alldata.jpeg", plot = grid.arrange(output1a$plot), width = 10, height = 7)

output1b <- TheilSen(
  selectByDate(trend_data, year = (reporting_year-10):reporting_year),
  pollutant = "pm10",
  xlab = "Year",
  ylab = expression(Richmond ~ Central ~ PM[10] ~ (mu * g / m^{
    3
  })),
  deseason = TRUE,
  slope.percent = FALSE
)

ggsave("outputs/aq_richmond_pm10_trend_last10years.jpeg", plot = grid.arrange(output1b$plot), width = 10, height = 7)

output1c <- TheilSen(
  selectByDate(trend_data, year = (reporting_year-6):reporting_year),
  pollutant = "pm10",
  xlab = "Year",
  ylab = expression(Richmond ~ Central ~ PM[10] ~ (mu * g / m^{
    3
  })),
  deseason = TRUE,
  slope.percent = FALSE
)
output1c$data[[1]]

ggsave("outputs/aq_richmond_pm10_trend_last6years.jpeg", plot = grid.arrange(output1c$plot), width = 10, height = 7)

# Modelling continues below














# Below is some modelling I undertook to estimate the PM10 values based on meteorological parameters.

# Kruskal-Wallis
kruskal.test(value ~ reporting_year, data = aq_day_richmond_pm10)
# Interpretation: as the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.
pwt <- pairwise.wilcox.test(aq_day_richmond_pm10$value, aq_day_richmond_pm10$year,
  p.adjust.method = "BH"
)
ifelse(pwt$p.value < 0.05, 1, 0) # Interpretation: the pairwise comparison shows that, any combination of years where p < 0.05  are significantly different.

#### Supervised Classification

pm10 <- aq_day_richmond_pm10 %>%
  filter(year >= 2010) %>%
  select(-normalised_pm) %>%
  mutate_at(vars(wind_kph_nhrs_less1, wind_kph_nhrs_less2, wind_kph_nhrs_less3, temp_degc_nhrs_less1, temp_degc_nhrs_less5, temp_degc_nhrs_less10), ~ replace_na(., 0)) %>%
  drop_na() %>%
  # select(pm10, wind_kph_24h_avg, temp_degc_4h_avg) %>%  # reducing variables initially for first model development
  mutate(
    year_month = factor(format(date, "%Y-%b"), levels = unique(format(date, "%Y-%b")), ordered = TRUE),
    pm10 = if_else(value <= 50, "pm10_below_50", "pm10_above_50"),
    pm10 = factor(pm10, levels = c("pm10_below_50", "pm10_above_50"), labels = c("PM10 below 50", "PM10 above 50"))
  ) # covert to binary outcome for supervised classification

# Plot with temperature and wind -> PM10
p_wt_pm10_bymonth <- ggplot() +
  geom_point(data = pm10, aes(temp_degc_4h_avg, wind_kph_24h_avg, color = value, size = value), position = "jitter") +
  scale_color_distiller(expression(PM[10] ~ (mu * g / m^{
    3
  })), palette = "Spectral") +
  labs(
    x = expression(Temperature ~ (degree * C)), y = expression(Wind ~ Speed ~ (km / h)), title = "Daily Records Richmond Central at Plunket",
    caption = "Temperature: 4 hour average from 8 pm to 12 pm, Wind Speed: 24 hour average"
  ) +
  theme_bw() +
  scale_size(guide = "none", range = c(0.5, 2)) +
  facet_wrap(~year_month) +
  theme(strip.text = element_text(color = "white"), strip.background = element_rect(fill = "#273691"))
ggsave("outputs/aq_richmond_pm10_wind_temp_pm10_bymonth.jpeg", plot = p_wt_pm10_bymonth, width = 10, height = 7)

####################

pm10 %>%
  filter(pm10 == "PM10 below 50") %>%
  group_by(reporting_year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  ggplot(aes(reporting_year, n)) +
  geom_point() +
  theme_bw()

model_data <- pm10 %>%
  select(-c(site, measurement, reporting_period, reporting_year, value)) %>%
  select(date, pm10, temp_degc_4h_avg, wind_kph_24h_avg, rainfall)

model_data <- merge(model_data, data.frame(date = seq(min(pm10$date), max(pm10$date), 1)), all = TRUE) %>% # add missing dates
  mutate(
    temp_degc_4h_avg_rollingm3day = rollapplyr(temp_degc_4h_avg, 3, mean, na.rm = TRUE, partial = TRUE),
    temp_degc_4h_avg_rollingm5day = rollapplyr(temp_degc_4h_avg, 5, mean, na.rm = TRUE, partial = TRUE),
    wind_kph_24h_avg_rollingm3day = rollapplyr(wind_kph_24h_avg, 3, mean, na.rm = TRUE, partial = TRUE),
    wind_kph_24h_avg_rollingm5day = rollapplyr(wind_kph_24h_avg, 5, mean, na.rm = TRUE, partial = TRUE),
    rainfall_rollingm3day = rollapplyr(rainfall, 3, mean, na.rm = TRUE, partial = TRUE),
    rainfall_rollingm5day = rollapplyr(rainfall, 5, mean, na.rm = TRUE, partial = TRUE)
  ) %>%
  drop_na()

pm10_exceedance <- subset(model_data, pm10 == "PM10 above 50")
pm10_no_exceedance <- subset(model_data, pm10 == "PM10 below 50")

p_pm10_exceedance <-
  ggplot(pm10_exceedance, aes(temp_degc_4h_avg_rollingm3day, wind_kph_24h_avg)) +
  geom_hex(aes(color = pm10), bins = 10, alpha = 0.7) +
  geom_point(color = "black", size = 0.1, shape = 3, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(limits = c(0, 35), position = "right") +
  # coord_fixed() +
  scale_color_discrete(guide = "none") +
  scale_fill_continuous(name = "Measurement \nCount", type = "viridis") +
  labs(x = expression(Temperature ~ (degree * C)), y = expression(Wind ~ Speed ~ (km / h))) +
  theme_bw() +
  theme(legend.position = "left", legend.key.size = unit(0.5, "cm")) + # legend.position=c(0.05, 0.98) legend.justification=c(0,1)
  facet_wrap(~pm10) +
  theme(strip.text = element_text(color = "white"), strip.background = element_rect(fill = "#273691"))

p_pm10_no_exceedance <-
  ggplot(pm10_no_exceedance, aes(temp_degc_4h_avg_rollingm3day, wind_kph_24h_avg)) +
  geom_hex(aes(color = pm10), bins = 10, alpha = 0.7) +
  geom_point(color = "black", size = 0.1, shape = 3, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 35), guide = "none") +
  scale_y_continuous(limits = c(0, 35), position = "right") +
  # coord_fixed() +
  scale_color_discrete(guide = "none") +
  scale_fill_continuous(name = "Measurement \nCount", type = "viridis") +
  labs(x = expression(Temperature ~ (degree * C)), y = expression(Wind ~ Speed ~ (km / h))) +
  theme_bw() +
  theme(legend.position = "left", legend.key.size = unit(0.5, "cm")) + # legend.position=c(0.05, 0.98) legend.justification=c(0,1)
  facet_wrap(~pm10) +
  theme(strip.text = element_text(color = "white"), strip.background = element_rect(fill = "#273691"))

t_min <- theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank()
)

tempdensity_exceedance <- ggplot(pm10_exceedance, aes(temp_degc_4h_avg_rollingm3day)) +
  geom_density(alpha = .5, fill = "#273691") +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_reverse() +
  labs(x = "", y = "Density") +
  scale_fill_manual(values = c("#999999")) +
  theme(legend.position = "none") +
  theme_bw() +
  t_min

winddensity_exceedance <- ggplot(pm10_exceedance, aes(wind_kph_24h_avg)) +
  geom_density(alpha = .5, fill = "#273691") +
  scale_x_continuous(limits = c(0, 35)) +
  labs(x = "", y = "Density") +
  scale_fill_manual(values = c("#999999")) +
  coord_flip() +
  theme(legend.position = "none") +
  theme_bw() +
  t_min

tempdensity_no_exceedance <- ggplot(pm10_no_exceedance, aes(temp_degc_4h_avg_rollingm3day)) +
  geom_density(alpha = .5, fill = "#273691") +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_reverse() +
  labs(x = "", y = "Density") +
  scale_fill_manual(values = c("#999999")) +
  theme(legend.position = "none") +
  theme_bw() +
  t_min

winddensity_no_exceedance <- ggplot(pm10_no_exceedance, aes(wind_kph_24h_avg)) +
  geom_density(alpha = .5, fill = "#273691") +
  scale_x_continuous(limits = c(0, 35)) +
  labs(x = "", y = "Density") +
  scale_fill_manual(values = c("#999999")) +
  coord_flip() +
  theme(legend.position = "none") +
  theme_bw() +
  t_min

# Set grid layout through design
design <- "
  12
  3#
"

p_wt_pm10_byexceedance <- p_pm10_exceedance + winddensity_exceedance +
  tempdensity_exceedance + plot_layout(design = design, widths = c(5, 1), heights = c(5, 1)) +
  plot_annotation(
    title = "Daily Records Richmond Central at Plunket Exceedances 2010-2023",
    caption = "Temperature: 4 hour average from 8 pm to 12 pm for exceedance day and 2 days prior, Wind Speed: 24 hour average"
  )

p_wt_pm10_bynoexceedance <- p_pm10_no_exceedance + winddensity_no_exceedance +
  tempdensity_no_exceedance + plot_layout(design = design, widths = c(5, 1), heights = c(5, 1)) +
  plot_annotation(
    title = "Daily Records Richmond Central at Plunket Non-Exceedances 2010-2023",
    caption = "Temperature: 4 hour average from 8 pm to 12 pm for exceedance day and 2 days prior, Wind Speed: 24 hour average"
  )

ggsave("outputs/aq_richmond_pm10_wind_temp_pm10_byexceedance.jpeg", plot = p_wt_pm10_byexceedance, width = 7, height = 7)
ggsave("outputs/aq_richmond_pm10_wind_temp_pm10_bynoexceedance.jpeg", plot = p_wt_pm10_bynoexceedance, width = 7, height = 7)

model_data_pre2022 <- model_data %>%
  filter(date < ymd("20220101")) %>%
  select(-c(date))

model_data_2022 <- model_data %>%
  filter(date >= ymd("20220101")) %>%
  select(-c(date))

# Split into training and test datasets
train_size <- round(0.75 * nrow(model_data_pre2022), 0)
split <- sample(c(rep(0, train_size), rep(1, nrow(model_data_pre2022) - train_size)))
table(split)
train_data <- model_data_pre2022[split == 0, ]
test_data <- model_data_pre2022[split == 1, ]

##########################
library(rpart)
library(rpart.plot)

prune_control <- rpart.control(maxdepth = 10) # minsplit = 10
m <- rpart(pm10 ~ ., data = train_data, method = "class", control = prune_control)

rpart.plot(m)
plotcp(m)
m_pruned <- prune(m, cp = 0.075)
summary(m_pruned)
rpart.plot(m_pruned, type = 3, box.palette = c("green", "red"), fallen.leaves = TRUE)

test_data$predict <- predict(m_pruned, test_data, type = "class")
mean(test_data$predict == test_data$pm10, na.rm = TRUE) # this is a case of predicting rare events, need to revisit.

# True positive accuracy
positive <- test_data %>% filter(pm10 == "PM10 above 50")
mean(positive$predict == positive$pm10, na.rm = TRUE)

model_data$predict <- predict(m_pruned, model_data, type = "class")

pred_out <- model_data %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(count = sum(predict == "PM10 above 50"))
clipr::write_clip(pred_out)

ggplot(pred_out, mapping = aes(year, count)) +
  geom_col()

##########################
library(randomForest)

mrf <- randomForest(pm10 ~ .,
  data = train_data,
  ntree = 500
)

test_data$predict <- predict(mrf, test_data, type = "class")
mean(test_data$predict == test_data$pm10, na.rm = TRUE)

model_data$predict_mrf <- predict(mrf, model_data, type = "class")

pred_out_mrf <- model_data %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(count = sum(predict_mrf == "PM10 above 50"))
clipr::write_clip(pred_out_mrf)
##########################

summary_stats_meteo <- model_data %>%
  mutate(
    year = year(date),
    month = as.numeric(month(date)),
    reporting_year = if_else(month > 8, year + 1, year)
  ) %>%
  select(reporting_year, predict) %>%
  group_by(reporting_year) %>%
  summarise(
    low_exceedance_days = sum(predict == "PM10 below 50", na.rm = TRUE),
    high_exceedance_days = sum(predict == "PM10 above 50", na.rm = TRUE)
  )

summary_out <- summary_stats %>% left_join(summary_stats_meteo, by = "reporting_year")
