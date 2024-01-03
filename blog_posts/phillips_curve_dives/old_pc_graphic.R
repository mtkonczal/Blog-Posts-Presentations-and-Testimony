# This is code for loading data and preparing analysis for Phillips Curve blog post.
# Mike Konczal
# August 15th, 2023
# Install and load required packages
library(quantmod)
library(tidyverse)
library(lubridate)
library(latex2exp)
library(readxl)
library(zoo)
library(ggrepel)

# Get the 4 quarters that move off the maximum date.
start_month <- month(max(pc_analysis$date))
quarters <- ((seq(start_month, start_month + 9, by=3) - 1) %% 12) + 1


pc_analysis <- pc_analysis %>% filter(month(date) %in% quarters) %>%
  mutate(FRB_post1991 = year(date)>=1992,
         FRB_post1991 = if_else(FRB_post1991,FRB_exp, as.numeric(NA)))

# Create lagged variables so they aren't cut off when we start in 1992.
pc_analysis <- pc_analysis %>%
  mutate(lagged_1 = lag(core_pce_changeA, 1),
         lagged_2 = lag(core_pce_changeA, 2))

#### Section 2: Phillips Curve ####
# Run regression: PCE_Core_Inflation ~ lagged PCE_Core_Inflation + Expected_Inflation + Noncyclical_Unemployment
pc_analysis_1991 <- pc_analysis %>% filter(year(date)>=1992, year(date)<=2019)
model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp +
              unrate_slack, data = pc_analysis_1991)
summary(model)

model2 <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp +
               unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01" & pc_analysis$date>="1970-01-01",])
summary(model2)

pc_analysis$predicted_1980_2019 <- predict(model, newdata = pc_analysis)
pc_analysis$predicted_1970_2019 <- predict(model2, newdata = pc_analysis)

date_breaks <- sort(unique(pc_analysis$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 24)]

pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation"), size=1.2) +
  geom_line(aes(y = predicted_1980_2019, color = "Predicted Inflation, 1991- training data")) +
  geom_line(aes(y = predicted_1970_2019, color = "Predicted Inflation, 1970- training data")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; FRB/US data for 1970-. U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

#ggsave("graphics/pc_model.png", dpi="retina", width = 12, height=6.75, units = "in")
