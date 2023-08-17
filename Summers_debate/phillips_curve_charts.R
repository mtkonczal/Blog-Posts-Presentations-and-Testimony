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

#### SECTION 1: Preparing Data ####

# Function for downloading data from FRED
prep_FRED_data <- function(x) {
  getSymbols(x, src="FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

# List of variables to download
fred_variables <- c("EXPINF5YR","UNRATE","NROU","PCEPILFE","MICH","GDP","JTSJOR","MEDCPIM158SFRBCLE")

# Download process, doing some manipulations so the characters become variable names
for(i in fred_variables){
  prep_FRED_data(i)
  data <- prep_FRED_data(i)
  assign(tolower(i), data, envir = .GlobalEnv)
  rm(data)
}

pc_data <- get(tolower(fred_variables[1]))
# Joining them all into one dataset. This dataset is monthly, with quarterly values missing dates as NA.
for(i in fred_variables[-1]) {
  pc_data <- full_join(pc_data, get(tolower(i)), by="date")
}
pc_data <- pc_data %>% arrange(date) %>%
  rename(core_pce = pcepilfe,
         job_openings = jtsjor,
         median_cpi = medcpim158sfrbcle)

# Additional data sadly not on the FRED website.
# New York Supply Chain Index
supply_chain <-read_csv("data/supply_chain.csv")
supply_chain$date <- as.Date(paste(supply_chain$year, supply_chain$month,1, sep = "-"), "%Y-%m-%d")
supply_chain <- supply_chain %>% select(-month, -year)
pc_data <- pc_data %>% left_join(supply_chain, by="date")

long_exp <- read_delim("data/LONGBASE.TXT") %>%
  select(OBS, PTR) %>%
  mutate(year = as.numeric(substr(OBS, 1, 4)), quarter = as.numeric(substr(OBS, 6, 6))) %>%
  mutate(month = quarter*2+quarter-2) %>%
  mutate(date = as.Date(paste(year, month,1, sep = "-"), "%Y-%m-%d")) %>%
  select(date, FRB_exp = PTR) %>%
  filter(year(date) > 1975)
pc_data <- pc_data %>% left_join(long_exp, by="date")

long_jo <- read_csv("data/historical jolts.csv") %>% select(date = date, job_opening_rate_long = job_opening_rate)
long_jo <- long_jo %>% full_join(jtsjor, by="date") %>%
  mutate(actual_opening = if_else(!is.na(jtsjor), jtsjor, job_opening_rate_long)) %>%
  select(date, long_openings = actual_opening)
pc_data <- pc_data %>% left_join(long_jo, by="date")

# Last, get the changes and recreate some new variables
pc_analysis <- pc_data %>%
  mutate(
    core_pce_changeA = (core_pce/lag(core_pce,3))^4 - 1,
    core_pce_changeA = core_pce_changeA*100,
  ) %>%
  filter(!is.na(core_pce_changeA))


# For some quarterly data, we just go ahead and fill in the gaps with the previous version.
# Future updates TKTK might extrapolate in-between, not sure best process.
# Also there must be a way to not do this in a for loop but we're just going to move right along.
pc_analysis$nrou <- na.locf(pc_analysis$nrou, na.rm = FALSE)
pc_analysis$gdp <- na.locf(pc_analysis$gdp, na.rm = FALSE)
pc_analysis$FRB_exp <- na.locf(pc_analysis$FRB_exp, na.rm = FALSE)

pc_analysis$unrate_slack = pc_analysis$unrate - pc_analysis$nrou

start_month <- month(max(pc_analysis$date))
quarters <- ((seq(start_month, start_month + 9, by=3) - 1) %% 12) + 1

pc_analysis <- pc_analysis %>% filter(month(date) %in% quarters)

#### Section 2: Phillips Curve ####
# Run regression: PCE_Core_Inflation ~ lagged PCE_Core_Inflation + Expected_Inflation + Noncyclical_Unemployment
model <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + expinf5yr +
              unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01",])
summary(model)

model2 <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + FRB_exp +
              unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01",])
summary(model2)

pc_analysis$predicted_1980_2019 <- predict(model, newdata = pc_analysis)
pc_analysis$predicted_1970_2019 <- predict(model2, newdata = pc_analysis)

pc_analysis %>% filter(year(date)>1982) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation")) +
  geom_line(aes(y = predicted_1980_2019, color = "Predicted Inflation, 1982-2019")) +
  geom_line(aes(y = predicted_1970_2019, color = "Predicted Inflation, 1976-2019")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation, Fed's Phillips Curve", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Model is trained on quarterly data from 1982-2019 (red) or 1975-2019 (purple), Fed's model of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Model from (Yellen 2017). Cleveland Fed 5-year expected inflation used for expectations. u-star from CBO. Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1982-2019" = "red", "Predicted Inflation, 1976-2019" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot")



pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation")) +
  geom_line(aes(y = predicted_1980_2019, color = "Predicted Inflation")) +
  geom_line(aes(y = predicted_1970_2019, color = "1970s Predicted Inflation")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is quarterly 1982-2019 (red), 1975-2019 (purple), quarters starting in July, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Cleveland Fed 5-year expected inflation used for expectations. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation" = "red","1970s Predicted Inflation" = "purple")) +
  theme_minimal() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot")


#### Jolts Graphic ####

# Create the new variables
pc_analysis <- pc_data %>%
  mutate(
    core_pce_changeA = (core_pce/lag(core_pce,3))^4 - 1,
    core_pce_changeA = core_pce_changeA*100,
  ) %>%
  filter(!is.na(core_pce_changeA))

pc_analysis$nrou <- na.locf(pc_analysis$nrou, na.rm = FALSE)
pc_analysis$gdp <- na.locf(pc_analysis$gdp, na.rm = FALSE)
pc_analysis$FRB_exp <- na.locf(pc_analysis$FRB_exp, na.rm = FALSE)
pc_analysis$unrate_slack = pc_analysis$unrate - pc_analysis$nrou

pc_analysis$v_u_ratio = pc_analysis$long_openings/pc_analysis$unrate
pc_analysis$v_u_ratio_star = pc_analysis$long_openings/pc_analysis$unrate - pc_analysis$long_openings/pc_analysis$nrou

start_month <- month(max(pc_analysis$date))
quarters <- ((seq(start_month, start_month + 9, by=3) - 1) %% 12) + 1

pc_analysis <- pc_analysis %>% filter(month(date) %in% quarters)

model_uv <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + expinf5yr +
                 v_u_ratio, data = pc_analysis[pc_analysis$date<"2020-01-01",])

model_u <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + expinf5yr +
               unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01",])

pc_analysis$predicted_1980_2019_vu <- predict(model_uv, newdata = pc_analysis)
pc_analysis$predicted_1980_2019_u <- predict(model_u, newdata = pc_analysis)

pc_analysis %>% filter(year(date)>1980) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation")) +
  geom_line(aes(y = predicted_1980_2019_vu, color = "Predicted Inflation - v/u - v/u*")) +
  geom_line(aes(y = predicted_1980_2019_u, color = "Predicted Inflation - u - u*")) +
  #geom_line(aes(y = predicted_1970_2019_vu, color = "1970s Predicted Inflation n/u")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation - v/u Phillips Curve", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is quarterly 1982-2019, quarters starting in July, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (v/u) or (u-u*)$)"),
       caption="Cleveland Fed 5-year expected inflation used for expectations. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation - u - u*" = "red","Predicted Inflation - v/u - v/u*" = "purple")) +
  theme_minimal() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot")


pc_analysis %>%
  filter(year(date)>1984 & year(date)<2020) %>%
  select(unrate_slack) %>%
  summarize(mean = mean(unrate_slack), median = median(unrate_slack))
## Larry Ball stuff
pc_data
pc_analysis2 <-
  pc_data %>%
  filter(!is.na(median_cpi)) %>%
  select(-mich, -gdp, -nrou) %>%
  mutate(v_u_ratio = long_openings/unrate,
         avg_v_u = v_u_ratio + lag(v_u_ratio,1) + lag(v_u_ratio,2) + lag(v_u_ratio,3),
         avg_v_u = avg_v_u/4) %>%
  mutate(median_cpi_unA = median_cpi/100 + 1,
         median_cpi_unA = median_cpi_unA^(1/12),
         median_cpi_index = cumprod(median_cpi_unA),
         median_cpi_unA = median_cpi_unA/lag(median_cpi_unA,12)-1) %>%
  mutate(core_inflation_gap = expinf5yr - median_cpi_unA) %>%
  select(date, avg_v_u, core_inflation_gap, median_cpi_unA, median_cpi) %>%
  filter(!is.na(avg_v_u)) %>%
  mutate(pandemic = year(date)>=2020) %>%
  mutate(pandemic_value = if_else(pandemic, avg_v_u, as.numeric(NA)),
         last_value = if_else(date==max(date), format(date, "%B\n%Y"), as.character(NA))) %>%
  ggplot(aes(avg_v_u, median_cpi, color=pandemic, label=last_value)) + geom_point() +
  geom_path(aes(pandemic_value, median_cpi)) + geom_text_repel(show.legend = FALSE) +
  labs(title="What happened?", subtitle="Figure 3 from the 'Scariest Economics Paper of 2022' (-Jason Furman). I have not done the fancy adjustments yet. Monthly.") +
  theme(legend.position = "NONE")
