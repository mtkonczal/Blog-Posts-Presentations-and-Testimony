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

long_exp <- read_delim("data/LONGBASE.TXT") %>%
  select(OBS, PTR) %>%
  mutate(year = as.numeric(substr(OBS, 1, 4)), quarter = as.numeric(substr(OBS, 6, 6))) %>%
  mutate(month = quarter*2+quarter-2) %>%
  mutate(date = as.Date(paste(year, month,1, sep = "-"), "%Y-%m-%d")) %>%
  select(date, FRB_exp = PTR) %>%
  filter(year(date) > 1970)
pc_data <- pc_data %>% left_join(long_exp, by="date")

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
               unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01",])
summary(model2)

pc_analysis$predicted_1980_2019 <- predict(model, newdata = pc_analysis)
pc_analysis$predicted_1970_2019 <- predict(model2, newdata = pc_analysis)

date_breaks <- sort(unique(pc_analysis$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 24)]

pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation")) +
  geom_line(aes(y = predicted_1980_2019, color = "Predicted Inflation, 1982- training data")) +
  geom_line(aes(y = predicted_1970_2019, color = "Predicted Inflation, 1970- training data")) +
  labs(title = "Hey, We're Doing This! Actual vs. Predicted PCE Core Inflation", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1982-2019 (red), 1970-2019 (purple), quarterly backwards from July 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Cleveland Fed 5-year expected inflation used for expectations for 1982-; FRB/US data for 1970-. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1982- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

ggsave("graphics/g1_core.png", dpi="retina", width = 12, height=6.75, units = "in")

write_csv(pc_analysis, "data/pc_analysis.csv")