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


# List of variables to download
fred_variables <- c("UNRATE","NROU","PCEPILFE","UNEMPLOY","JTSJOL","IREXPET","A255RC1Q027SBEA","GDP", "JTSQUL","PAYEMS","JTSQUR")


#### SECTION 1: Preparing Data ####
# COMMENT ME OUT TOMORROW
#long_pce <- prep_FRED_data("PCEPILFE") %>% rename(pce_index = pcepilfe)

#long_pce_pc <- long_pce %>%
#  select(date, pce_index = DataValue)


# Function for downloading data from FRED
prep_FRED_data <- function(x) {
  getSymbols(x, src="FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}


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


long_exp <- read_delim("data/LONGBASE.TXT") %>%
  select(OBS, PTR) %>%
  mutate(year = as.numeric(substr(OBS, 1, 4)), quarter = as.numeric(substr(OBS, 6, 6))) %>%
  mutate(month = quarter*2+quarter-2) %>%
  mutate(date = as.Date(paste(year, month,1, sep = "-"), "%Y-%m-%d")) %>%
  select(date, FRB_exp = PTR) %>%
  filter(year(date) > 1970)

gscpi <- read_csv("data/gscpi.csv")

# Data Fixing goes here
pc_data <- pc_data %>%
  rename(pce_index = pcepilfe,
         import_inflation = irexpet,
         import_size = a255rc1q027sbea)

pc_analysis <- pc_data %>%
  left_join(long_exp, by="date") %>%
  left_join(gscpi, by="date") %>%
  #left_join(long_pce, by="date") %>%
  mutate(
    core_pce_changeA = (pce_index/lag(pce_index,3))^4 - 1,
    core_pce_changeA = core_pce_changeA*100,
    import_inflation_changeA = (import_inflation/lag(import_inflation,3))^4-1,
    import_inflation_changeA = import_inflation_changeA*100
  ) %>%
  filter(!is.na(core_pce_changeA))


# For some quarterly data, we just go ahead and fill in the gaps with the previous version.
# Future updates TKTK might extrapolate in-between, not sure best process.
# Also there must be a way to not do this in a for loop but we're just going to move right along.
pc_analysis$nrou <- na.locf(pc_analysis$nrou, na.rm = FALSE)
pc_analysis$FRB_exp <- na.locf(pc_analysis$FRB_exp, na.rm = FALSE)
pc_analysis$gdp <- na.locf(pc_analysis$gdp, na.rm = FALSE)
pc_analysis$import_size <- na.locf(pc_analysis$import_size, na.rm = FALSE)

pc_analysis$unrate_slack = pc_analysis$unrate - pc_analysis$nrou
pc_analysis$v_u = pc_analysis$jtsjol / pc_analysis$unemploy
pc_analysis$v_u_squared = pc_analysis$v_u^2

pc_analysis$RFIM = (pc_analysis$import_inflation_changeA - pc_analysis$core_pce_changeA) * pc_analysis$import_inflation / pc_analysis$gdp

pc_analysis$quit_rate = pc_analysis$jtsqul / pc_analysis$payems

start_month <- month(max(pc_analysis$date))
start_month <- 10
quarters <- ((seq(start_month, start_month + 9, by=3) - 1) %% 12) + 1

pc_analysis <- pc_analysis %>% filter(month(date) %in% quarters)

date_breaks <- sort(unique(pc_analysis$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 8)]

pc_analysis <- pc_analysis %>%
  mutate(lagged_1 = lag(core_pce_changeA, 1),
         lagged_2 = lag(core_pce_changeA, 2))

