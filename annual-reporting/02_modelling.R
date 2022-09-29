library(knitr)

source("01_load_aq_data.R")

site <- "AQ Richmond Central at Plunket"
aq_day_richmond_pm10 <- filter(aq_day, site == !!site & measurement == "PM10") %>% 
  mutate(year = as.factor(year(date)))

aq_day_richmond_pm10
View(aq_day_richmond_pm10)


# summary stats

summary_stats <- group_by(aq_day_richmond_pm10, year) %>%
  summarise(
    count = n(),
    count_non_na = sum(!is.na(value)),
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
ifelse(pwt$p.value < 0.05, 1, 0)
# Interpretation: the pairwise comparison shows that, any combination of years where p < 0.05  are significantly different.


