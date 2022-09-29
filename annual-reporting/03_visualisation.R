library(glue)
library(patchwork)

source("02_modelling.R")

aq_day_reporting_period <- filter(aq_day, reporting_period == 1)

labels = tribble(~code, ~x, ~y, ~label,
                "naqs_pm10", min(aq_day_reporting_period$date), 50, National~Air~Quality~Standard~PM10~(50~mu*g/m^{3}),
                "mfe_pm10",  min(aq_day_reporting_period$date), 20, MfE~2005~PM10~guideline~(20~mu*g/m^{3}),
                "who_pm2p5", min(aq_day_reporting_period$date), 15, WHO~2021~PM2.5~guideline~(15~mu*g/m^{3}))
p1 <- ggplot() +
  geom_step(data = aq_day_reporting_period , aes(date, value, color = measurement), size = 0.5) +
  labs(x = "", y = expression(PM~(mu*g/m^{3})), title = "PM Daily") +
  geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 20, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 15, color = "orange", linetype = "dashed") +
  geom_text(data = labels, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.02), size = 2, color = "black", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "red", "PM2.5" = "orange"), labels = c(PM10 = expression(paste(~PM_10)), PM2.5 = expression(paste(PM_2.5)))) +
  scale_x_date(date_labels = "%Y-%b") +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 1)) +
  theme_bw() +
  facet_wrap(~site, ncol = 1)

ggsave("outputs/p1.jpeg", plot = p1, width = 10, height = 18)

###### Richmond daily PM10 concentrations

aq_day_richmond_pm10_reporting_period <- filter(aq_day_richmond_pm10, reporting_period == 1)

p_aq_day_richmond_pm10 <- ggplot() +
  geom_col(data = aq_day_richmond_pm10_reporting_period , aes(date, value, color = measurement), size = 0.5, fill = "red", alpha = 0.4) +
  labs(x = "", y = expression(PM[10]~(mu*g/m^{3})), title = glue({site})) +
  geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
  geom_text(data = filter(labels, code == "naqs_pm10"), mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 4, color = "black", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "red"), labels = c(PM10 = expression(paste(~PM[10])), PM2.5 = expression(paste(PM_2.5)))) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 55), expand = c(0, 0)) +
  theme_bw() 

ggsave("outputs/aq_day_richmond_pm10.jpeg", plot = p_aq_day_richmond_pm10, width = 10, height = 7)

###### Richmond annual daily PM10 distribution
site <- "AQ Richmond Central at Plunket"


label1 <- filter(labels, code == "naqs_pm10") %>% 
  mutate(x = as.factor(2006))

p_aq_day_richmond_pm10_annual_boxplot <- ggplot() +
  geom_boxplot(data = aq_day_richmond_pm10 , aes(year, value), size = 0.5,  color = "red", fill = "red", alpha = 0.4) +
  labs(x = "", y = expression(PM[10]~(mu*g/m^{3})), title = glue({site})) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed") +
  geom_text(data = label1, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 4, color = "black", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "red"), labels = c(PM10 = expression(paste(~PM[10])), PM2.5 = expression(paste(PM_2.5)))) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(0, max(aq_day_richmond_pm10$value, na.rm = TRUE)*1.02), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p_aq_day_richmond_pm10_annual_violin <- ggplot() +
  geom_violin(data = aq_day_richmond_pm10 , aes(year, value), size = 0.5,  color = "red", fill = "red", alpha = 0.4) +
  labs(x = "", y = expression(PM[10]~(mu*g/m^{3})), title = glue({site})) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed") +
  geom_text(data = label1, mapping =  aes(x, y, label = label, vjust = -0.15, hjust = -0.0), size = 4, color = "black", parse = TRUE) +
  scale_color_manual(name = "Legend", values = c("PM10" = "red"), labels = c(PM10 = expression(paste(~PM[10])), PM2.5 = expression(paste(PM_2.5)))) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(0, max(aq_day_richmond_pm10$value, na.rm = TRUE)*1.02), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p_aq_day_richmond_pm10_annual <- p_aq_day_richmond_pm10_annual_boxplot / p_aq_day_richmond_pm10_annual_violin

ggsave("outputs/aq_annual_richmond_pm10.jpeg", plot = p_aq_day_richmond_pm10_annual, width = 10, height = 14)

