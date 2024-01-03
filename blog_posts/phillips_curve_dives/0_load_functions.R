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
library(broom)

data <- centered_pc
model <- pc_vu_analysis_model

#### Apply the value in the model
predict_contributions <- function(data, model) {
  # Extract model coefficients, excluding the intercept
  coefficients_df <- tidy(model) %>%
    filter(term != "(Intercept)")

  # Map coefficients to the relevant columns in the data
  contributions <- data %>%
    select(all_of(coefficients_df$term)) %>%
    mutate(across(everything(), ~ .x * coefficients_df$estimate[match(cur_column(), coefficients_df$term)]))
  # Add time variable (assuming your time series data has a 'time' column)
  contributions$date <- data$date

  # Pivot longer
  long_contributions <- contributions %>%
    pivot_longer(-date, names_to = "variable", values_to = "contribution")

  return(long_contributions)
}

center_mean <- function(df, start_year = NA, end_year = NA) {
  if (is.numeric(start_year)) {
    df <- df %>% filter(year(date) >= start_year)
  }
  if (is.numeric(end_year)) {
    df <- df %>% filter(year(date) <= end_year)
  }

  df_dates <- df$date
  centered_tibble <- df %>%
    select(-date) %>%
    mutate_all(~ . - mean(., na.rm = TRUE))

  centered_tibble$date <- df_dates

  return(centered_tibble)
}