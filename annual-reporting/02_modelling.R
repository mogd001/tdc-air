library(knitr)
library(tidyverse)
library(lubridate)

#source("01_load_aq_data.R")
#saveRDS(aq_day, file = "temp.RDS") 

aq_day <- readRDS("temp.RDS")

site <- "AQ Richmond Central at Plunket"
aq_day_richmond_pm10 <- filter(aq_day, site == !!site & measurement == "PM10") %>% 
  mutate(year = as.factor(year(date)))

aq_day_richmond_pm10
View(aq_day_richmond_pm10)

# summary stats
summary_stats <- group_by(aq_day_richmond_pm10, year) %>%
  summarise(
    count = n(),
    count_non_na = sum(!is.na(value), na.rm = TRUE),
    count_exceedance = sum(value > 50, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  )

kable(summary_stats)

# Kruskal-Wallis
kruskal.test(value ~ year, data = aq_day_richmond_pm10)
# Interpretation: as the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.
pwt <- pairwise.wilcox.test(aq_day_richmond_pm10$value, aq_day_richmond_pm10$year,
                     p.adjust.method = "BH")
ifelse(pwt$p.value < 0.05, 1, 0) # Interpretation: the pairwise comparison shows that, any combination of years where p < 0.05  are significantly different.

#### Supervised Classification
all_data <- aq_day_richmond_pm10 %>% 
  mutate(pm10 = value) %>% 
  mutate_at(vars(wind_mps_nhrs_less1, wind_mps_nhrs_less2, wind_mps_nhrs_less3, temp_degc_nhrs_less1, temp_degc_nhrs_less5, temp_degc_nhrs_less10), ~replace_na(., 0)) %>% 
  drop_na() %>% 
  #select(pm10, wind_mps_24h_avg, temp_degc_4h_avg) %>%  # reducing variables initially for first model development
  mutate(pm10 = if_else(pm10 <= 50, "pm10_below_50", "pm10_above_50")) %>% # covert to binary outcome for supervised classification
  mutate(pm10 = factor(pm10, levels = c("pm10_below_50", "pm10_above_50")))

ggplot(all_data, aes(wind_mps_24h_avg, temp_degc_4h_avg, value)) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  facet_wrap(~pm10)

all_data %>% 
  filter(pm10 == "pm10_above_50") %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(year, n)) +
  geom_point() +
  theme_bw() +
  
model_data <- all_data %>% 
  select(-c(date, site, measurement, reporting_period, year, value)) %>% 
  select(pm10, temp_degc_4h_avg, wind_mps_24h_avg)

# Split into training and test dataset
train_size <- round(0.75 * nrow(model_data), 0)
split <- sample(c(rep(0, train_size), rep(1, nrow(model_data) - train_size)))
table(split) 
train_data <- model_data[split == 0, ]      
test_data <- all_data[split == 1, ]

##########################
library(rpart)
library(rpart.plot)

prune_control <- rpart.control(maxdepth = 10) # minsplit = 10
m <- rpart(pm10 ~ ., data = train_data, method = "class", control = prune_control)

rpart.plot(m)
plotcp(m)
m_pruned <- prune(m, cp = 0.075)
rpart.plot(m_pruned, type = 3, box.palette = c("green", "red"), fallen.leaves = TRUE)

test_data$predict <- predict(m_pruned, test_data, type = "class")
mean(test_data$predict == test_data$pm10, na.rm = TRUE)  # this is a case of predicting rare events, need to revisit.

##########################
library(randomForest)

mrf <- randomForest(pm10 ~ ., data = train_data,
                  ntree = 500)

test_data$predict <- predict(mrf, test_data, type = "class")
mean(test_data$predict == test_data$pm10, na.rm = TRUE)

##########################

all_data$predict <- predict(m_pruned, all_data, type = "class") 

summary_stats_meteo <- all_data %>% 
  select(year, predict) %>% 
  group_by(year) %>%
  summarise(
    low_exceedance = sum(predict == "pm10_below_50", na.rm = TRUE),
    high_exceedance = sum(predict == "pm10_above_50", na.rm = TRUE)
  )

summary_out <- summary_stats %>% left_join(summary_stats_meteo, by = "year")
# bar chart showing proportions