library(glue)
library(patchwork)

source("01_load_aq_data.R")
#source("02_modelling.R")

aq_day_reporting_period <- filter(aq_day, reporting_period == 1) %>% 
  mutate(year = year(date),
         month = as.numeric(month(date)),
         reporting_year = if_else(month > 8, year + 1, year))

labels <- tribble(~code, ~code_type, ~measurement, ~x, ~y, ~label,
                "daily_naqs_pm10", "daily", "PM10", min(aq_day_reporting_period$date), 50, National~Air~Quality~Standard~24~Hour~PM[10]~(50~mu*g/m^{3}),
                "daily_who_pm10", "daily", "PM10", min(aq_day_reporting_period$date), 45, WHO~2021~Guideline~24~Hour~PM[10]~(45~mu*g/m^{3}),
                "annual_who_pm10", "annual", "PM10", min(aq_day_reporting_period$date), 15, WHO~2021~Guideline~Annual~PM[10]~(15~mu*g/m^{3}),
                #"daily_naqs_pm2p5", "daily", "PM2.5", min(aq_day_reporting_period$date), 25, Draft~National~Air~Quality~Standard~24-Hour~PM[2.5]~(25~mu*g/m^{3}),
                "daily_who_pm2p5", "daily", "PM2.5", min(aq_day_reporting_period$date), 15, WHO~2021~Guideline~24~Hour~PM[2.5]~(15~mu*g/m^{3}),
                #"annual_naqs_pm2p5", "annual", "PM2.5", min(aq_day_reporting_period$date), 10, National~Air~Quality~Standard~24-Hour~PM[2.5]~(10~mu*g/m^{3}),
                "annual_who_pm2p5", "annual", "PM2.5", min(aq_day_reporting_period$date), 5, WHO~2021~Guideline~Annual~PM[2.5]~(5~mu*g/m^{3}))


labels_general <- tribble(~x, ~y, ~label,
                          ymd(20220601), 55, "Winter 2022")

###### All plots in one
labels_daily <- labels %>% filter(code_type == "daily")
  
p_reporting_period <- ggplot() +
  geom_col(data = aq_day_reporting_period , aes(date, value, fill = measurement), size = 0.5) +
  labs(x = "", y = expression(PM~(mu*g/m^{3})), title = "PM Daily") +
  
  geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 45, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 25, color = "orange", linetype = "dashed") +
  geom_hline(yintercept = 15, color = "orange", linetype = "dashed") +
  
  geom_text(data = labels_daily, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 2, color = "black", parse = TRUE) +
  scale_fill_manual(name = "Legend", values = c("PM10" = "red", "PM2.5" = "orange"), labels = c(PM10 = expression(paste(~PM[10])), PM2.5 = expression(paste(PM[2.5])))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 55), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(site~measurement)

ggsave("outputs/p1.jpeg", plot = p_reporting_period, width = 14, height = 10)

###### Richmond daily PM10 concentrations
site <- "AQ Richmond Central at Plunket"
aq_day_richmond_pm10_reporting_period <- filter(aq_day_reporting_period, site == !!site & measurement == "PM10")
labels_daily_richmond_pm10 <- labels %>% filter(code_type == "daily" & measurement == "PM10")

p_aq_day_richmond_pm10 <- ggplot() +
  annotate("rect", xmin = ymd(20220501), xmax = ymd(20220831), ymin = 0, ymax = 60, alpha = 0.1, fill = "black") + # add winter rectangle
  geom_label(data = labels_general, aes(x, y, label = label)) + 
  geom_col(data = aq_day_richmond_pm10_reporting_period , aes(date, value, color = measurement), size = 0.2, fill = "black", alpha = 0.2) +
  labs(x = "", y = expression(PM[10]~(mu*g/m^{3})), title = glue({site})) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 45, color = "black", linetype = "dashed") + 
  geom_text(data = labels_daily_richmond_pm10, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 3.5, color = "black", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "black"), labels = c(PM10 = expression(paste(~PM[10])))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position="none") 

ggsave("outputs/aq_day_richmond_pm10.jpeg", plot = p_aq_day_richmond_pm10, width = 10, height = 7)

df <- aq_day_richmond_pm10_reporting_period %>% 
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

summary_day_richmond_pm10_reporting_period <- as_tibble(cbind(nms = names(df), round(t(df), 0)))
colnames(summary_day_richmond_pm10_reporting_period) <- c("nms", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

summary_day_richmond_pm10_reporting_period
clipr::write_clip(summary_day_richmond_pm10_reporting_period)

annual_mean <- aq_day_richmond_pm10_reporting_period %>% 
  group_by(reporting_year) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE)
  )


###### Richmond daily PM2.5 concentrations
site <- "AQ Richmond Central at Plunket"
aq_day_richmond_pm2p5_reporting_period <- filter(aq_day_reporting_period, site == !!site & measurement == "PM2.5")
labels_daily_richmond_pm2p5 <- labels %>% filter(code_type == "daily" & measurement == "PM2.5")

p_aq_day_richmond_pm2p5 <- ggplot() +
  annotate("rect", xmin = ymd(20220501), xmax = ymd(20220831), ymin = 0, ymax = 60, alpha = 0.1, fill = "black") + # add winter rectangle
  geom_label(data = labels_general, aes(x, y, label = label)) + 
  geom_col(data = aq_day_richmond_pm2p5_reporting_period , aes(date, value, color = measurement), size = 0.2, fill = "black", alpha = 0.2) +
  labs(x = "", y = expression(PM[2.5]~(mu*g/m^{3})), title = glue({site})) +
  geom_hline(yintercept = 15, color = "black", linetype = "dashed") +
  geom_text(data = labels_daily_richmond_pm2p5, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 3.5, color = "black", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "black"), labels = c(PM10 = expression(paste(~PM[2.5])))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position="none") 

ggsave("outputs/aq_day_richmond_pm2p5.jpeg", plot = p_aq_day_richmond_pm2p5, width = 10, height = 7)

df <- aq_day_richmond_pm2p5_reporting_period %>% 
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

summary_day_richmond_pm2p5_reporting_period <- as_tibble(cbind(nms = names(df), round(t(df), 1)))
colnames(summary_day_richmond_pm2p5_reporting_period) <- c("nms", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

summary_day_richmond_pm2p5_reporting_period
clipr::write_clip(summary_day_richmond_pm2p5_reporting_period)

annual_mean <- aq_day_richmond_pm2p5_reporting_period %>% 
  group_by(reporting_year) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE)
  )
annual_mean






# 
# 
# 
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
