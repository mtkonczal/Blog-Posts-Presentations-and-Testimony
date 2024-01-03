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


#### Apply the value in the model
predict_contributions <- function(data, model) {
  # Extract model coefficients, excluding the intercept
  coefficients_df <- model %>%
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

draw_contribution <- function(contribution, full_df) {
  
  contribution %>%
    filter(year(date) > 2015) %>%
    left_join(full_df %>% select(date, core_pce_changeA), by = "date") %>%
    ggplot(aes(x = date, y = contribution, fill = variable)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Paired") +
    theme_classic() +
    geom_line(aes(date, core_pce_changeA), color = "#2D779C", size = 1.2) +
    theme(text = element_text(size = 24)) +
    labs(fill = "Variable", x = "Time", y = "Contribution") +
    guides(size = FALSE) +
    labs(
      subtitle = "Decomposed Phillips Curve regression for core PCE's difference from 2 percent, quarterly, model trained on data from 2001-2023.\nBlue line is actual value.",
      caption = "Mike Konczal, Roosevelt Institute."
    ) +
    scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
    theme(
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 17),
      legend.position = c(0.35, 0.75),
      legend.title = element_blank()
    )
}

adjusted_model_expectations <- function(model) {
  model <- tidy(model) %>%
    mutate(
      term = str_replace_all(term, "I\\(lagged_2 - lagged_1\\)", "lagged_2"),
      term = str_replace_all(term, "I\\(FRB_exp - lagged_1\\)", "FRB_exp")
    )

  subtracted <- 1 - model$estimate[model$term == "FRB_exp"] - model$estimate[model$term == "lagged_2"]
  model <- model %>% add_row(term = "lagged_1", estimate = subtracted, std.error = 0.0, statistic = 0.0, p.value = 1.0)

  return(model)
}