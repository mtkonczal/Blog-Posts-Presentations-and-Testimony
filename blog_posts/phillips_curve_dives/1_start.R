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

source("0_load_functions.R")
source("0_prepare_data.R")

# Get the 4 quarters that move off the maximum date.

# Create lagged variables so they aren't cut off when we start in 1992.
centered_pc <- center_mean(pc_analysis %>% select(date, v_u_squared, FRB_exp, RFIM, quit_rate, gscpi) %>% na.omit()) %>% na.omit()
centered_pc <- centered_pc %>% left_join(pc_analysis %>% select(date, core_pce_changeA) %>%
                                           mutate(core_pce_changeA = core_pce_changeA - 2,
                                                  lagged_1 = lag(core_pce_changeA,1),
                                                  lagged_2 = lag(core_pce_changeA,2)),
                                         by="date")


pc_vu_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM +
                             v_u_squared + quit_rate + gscpi, data = centered_pc)
summary(pc_vu_analysis_model)

tester <- predict_contributions(centered_pc, pc_vu_analysis_model)


tester %>% filter(year(date)> 2015) %>% left_join(centered_pc %>% select(date, core_pce_changeA), by="date") %>%
  ggplot(aes(x = date, y = contribution, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  geom_line(aes(date, core_pce_changeA), color="#2D779C", size=1.2) + 
  theme(text=element_text(size=24)) +
  labs(fill = "Variable", x = "Time", y = "Contribution") +
  guides(size = FALSE) +
  labs(subtitle = "Decomposed Phillips Curve regression for core PCE's difference from 2 percent, quarterly, model trained on data from 2001-2023.\nBlue line is actual value.",
       caption = "Mike Konczal, Roosevelt Institute.") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  theme(plot.title.position = "plot",
        plot.subtitle=element_text(size=17),
        legend.position = c(0.35, 0.75),
        legend.title = element_blank())


centered_pc$pc_v_u_predicted <- predict(pc_vu_analysis_model, newdata = centered_pc)


centered_pc %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = pc_v_u_predicted, size=0),stat="identity", size=0, fill = "lightblue") +
  geom_line(aes(y = core_pce_changeA),color = "#2D779C", size=1.2) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

#### Section 2: Phillips Curve ####
#### Model 1: Yellen (2015) ####
pc_flat_analysis <- pc_analysis %>% filter(year(date)>=1992, year(date)<=2019)
pc_flat_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM +
              unrate_slack, data = pc_flat_analysis)
summary(pc_flat_analysis_model)

pc_analysis$pc_flat_predicted <- predict(pc_flat_analysis_model, newdata = pc_analysis)


pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = pc_flat_predicted, size=0),stat="identity", size=0, fill = "lightblue") +
  geom_line(aes(y = core_pce_changeA),color = "#2D779C", size=1.2) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)


#### Model 2: v/u squared ####
pc_analysis <- pc_analysis %>% mutate(covid_era = year(date) >= 2020,
                                      quit_rate_star = quit_rate[date == "2019-11-01"],
                                      quit_rate_slack = quit_rate - quit_rate_star)

pc_vu_analysis <- pc_analysis %>% filter(year(date)>=1992)
pc_vu_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM +
                             quit_rate + gscpi - 1, data = pc_vu_analysis)
summary(pc_vu_analysis_model)

pc_analysis$pc_vu_predicted <- predict(pc_vu_analysis_model, newdata = pc_analysis)

pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = pc_vu_predicted, size=0),stat="identity", size=0, fill = "lightblue") +
  geom_line(aes(y = core_pce_changeA),color = "#2D779C", size=1.2) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

#ggsave("graphics/pc_model.png", dpi="retina", width = 12, height=6.75, units = "in")


#### Model 3: new york Fed ####
pc_gscpi_analysis <- pc_analysis %>% filter(year(date)>=1992, year(date)<=2022)
pc_gscpi_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM + gscpi +
                             unrate_slack, data = pc_gscpi_analysis)
summary(pc_gscpi_analysis_model)

pc_analysis$pc_gscpi_predicted <- predict(pc_gscpi_analysis_model, newdata = pc_analysis)

pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = pc_gscpi_predicted, size=0),stat="identity", size=0, fill = "lightblue") +
  geom_line(aes(y = core_pce_changeA),color = "#2D779C", size=1.2) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

#ggsave("graphics/pc_model.png", dpi="retina", width = 12, height=6.75, units = "in")

#### Model 4: both####
pc_full_analysis <- pc_analysis %>% filter(year(date)>=1992) %>% mutate(covid_era = year(date) >= 2020, v_u_supply = v_u_squared*RFIM)
pc_full_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM + gscpi + covid_era + v_u_supply +
                             v_u_squared, data = pc_full_analysis)
summary(pc_full_analysis_model)

pc_full_analysis$pc_full_predicted <- predict(pc_full_analysis_model, newdata = pc_full_analysis)

pc_full_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = pc_full_predicted, size=0),stat="identity", size=0, fill = "lightblue") +
  geom_line(aes(y = core_pce_changeA),color = "#2D779C", size=1.2) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

#ggsave("graphics/pc_model.png", dpi="retina", width = 12, height=6.75, units = "in")








####







#### Section 2: Phillips Curve ####
#### Model 1: Yellen (2015) ####
pc_flat_analysis <- pc_analysis %>% filter(year(date)>=1992, year(date)<=2019)
pc_flat_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM +
                               unrate_slack, data = pc_flat_analysis)
summary(pc_flat_analysis_model)

pc_analysis$pc_flat_predicted <- predict(pc_flat_analysis_model, newdata = pc_analysis)
pc_flat_analysis_model$pc_flat_predicted <- predict(pc_flat_analysis_model, newdata = pc_flat_analysis)

library(ggplot2)
library(dplyr)

# Example data

# Fit a linear model
# Calculate contributions

long_data_for_ggplot <- predict_contributions(pc_analysis, pc_flat_analysis_model)

pc_flat_analysis$contrib_lagged_1 <- pc_flat_analysis_model$coefficients["lagged_1"] * pc_flat_analysis$lagged_1
pc_flat_analysis$contrib_lagged_2 <- pc_flat_analysis_model$coefficients["lagged_2"] * pc_flat_analysis$lagged_2
pc_flat_analysis$contrib_FRB_exp <- pc_flat_analysis_model$coefficients["FRB_exp"] * pc_flat_analysis$FRB_exp
pc_flat_analysis$contrib_RFIM <- pc_flat_analysis_model$coefficients["RFIM"] * pc_flat_analysis$RFIM
pc_flat_analysis$contrib_unrate_slack <- pc_flat_analysis_model$coefficients["unrate_slack"] * pc_flat_analysis$unrate_slack
pc_flat_analysis$contrib_intercept <- pc_flat_analysis_model$coefficients["(Intercept)"]

pc_flat_analysis_model$pc_flat_predicted - pc_flat_analysis$contrib_lagged_1 - pc_flat_analysis$contrib_lagged_2 -
  pc_flat_analysis$contrib_FRB_exp - pc_flat_analysis$contrib_RFIM - pc_flat_analysis$contrib_unrate_slack -
  pc_flat_analysis$contrib_intercept

# Reshape the data (if necessary)
pc_flat_analysis_long <- pc_flat_analysis %>%
  select(date, starts_with("contrib")) %>%
  pivot_longer(cols = starts_with("contrib"), 
               names_to = "variable", values_to = "value")

# Plot
ggplot(pc_flat_analysis_long, aes(x = date, y = value, fill = variable)) +
  geom_bar(stat = "identity") # for stacked bar chart


pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = pc_flat_predicted, size=0),stat="identity", size=0, fill = "lightblue") +
  geom_line(aes(y = core_pce_changeA),color = "#2D779C", size=1.2) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)




library(tidyverse)
library(broom)
data <- pc_flat_analysis
model <- pc_flat_analysis_model
center <- TRUE
library(tidyverse)
library(broom)



# Example usage
# Assume your time series data is in `time_series_data` and your model is `model`
long_data_for_ggplot <- predict_contributions(pc_flat_analysis, pc_flat_analysis_model)


# Example usage
# Assume your time series data is in `time_series_data` and your model is `model`
centered_data <- pc_analysis %>% 
  mutate(across(-core_pce_changeA, ~ . - mean(.)))
long_data_for_ggplot <- predict_contributions(pc_analysis, pc_vu_analysis_model)


pc_flat_analysis <- pc_analysis %>% filter(year(date)>=1992, year(date)<=2019)
pc_flat_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM +
                               unrate_slack, data = centered_data)
summary(pc_flat_analysis_model)


long_data_for_ggplot <- predict_contributions(pc_analysis, pc_vu_analysis_model, center = TRUE)


pc_vu_analysis_model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp + RFIM +
                             v_u + gscpi - 1, data = pc_vu_analysis)
summary(pc_vu_analysis_model)

long_data_for_ggplot <- predict_contributions(pc_analysis, pc_vu_analysis_model, center = FALSE)

core_avg <- pc_analysis %>% filter(year(date) >= 2010) %>% summarize(mean(core_pce_changeA)) %>% pull

  long_data_for_ggplot %>% filter(year(date)> 2010) %>% left_join(pc_analysis %>% select(date, core_pce_changeA), by="date") %>%
    mutate(core_pce_changeA = core_pce_changeA - core_avg) %>%
    ggplot(aes(x = date, y = contribution, fill = variable)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_fill_brewer(palette = "Set1") +  # Set1 is a good general-purpose palette
    theme_minimal() +
    geom_line(aes(date, core_pce_changeA)) + 
    labs(fill = "Variable", x = "Time", y = "Contribution") +
    guides(size = FALSE)
  
  tail(long_data_for_ggplot,20)
