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
fred_variables <- c("UNRATE","PCEPILFE")

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
pc_data <- pc_data %>% arrange(date)



pc_data %>%
  rename(pce = pcepilfe) %>%
  mutate(unrate = unrate/100,
         pce = pce/lag(pce,3),
         pce = pce^4-1) %>%
  na.omit() %>%
  filter(month(date) %in% c(12,3,6,9)) %>%
  mutate(category = case_when(
    year(date) >= 1978 & year(date) <=1984 ~ "1978-1984",
    year(date) >= 2021 ~ "2021-"),
    category = as.factor(category)) %>%
  na.omit() %>%
  
  ggplot(aes(unrate, pce, label=date, color=category)) + geom_path() + theme_classic() +
  theme(legend.position = c(0.7,0.9))