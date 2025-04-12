library(tidyverse)
library(govMacroTools)
library(lubridate)
library(gt)

cps_data <- getBLSFiles("cps", "konczal@gmail.com")



# Graphic Table of Unemployment Increases ----
comparison_date <- "2021-04-01"
unrate_increase <- 0.01

cps <- cps_data %>% filter(seasonal == "S", periodicity_code == "M")
current <- max(cps$date)

# View(cps %>% filter(lfst_code %in% c("10","40"), date == "2023-05-01"))

# Select which groups
chart_lf_groups <- c("LNS11000000", "LNS11000001", "LNS11000002", "LNS11000003", "LNS11000006", "LNS11024887")
chart_unrate_groups <- c("LNS14000000", "LNS14000001", "LNS14000002", "LNS14000003", "LNS14000006", "LNS14024887")

# Get their labor force level
labor_force_levels <- cps %>% filter(series_id %in% chart_lf_groups) %>%
  select(ages_text, race_text, sexs_text, labor_force = value, date)

# unemployment rates - make sure to anchor in the labor force levels
chart_data <- cps %>%
  filter(series_id %in% chart_unrate_groups) %>%
  inner_join(labor_force_levels, by=c("ages_text", "race_text", "sexs_text", "date")) %>%
  select(series_id, ages_text, race_text, sexs_text, labor_force, unrate = value, date) %>%
  mutate(unrate = unrate / 100)


# Do comparison, proportionate to now.
chart_data %>%
  filter(date %in% c(current)) %>%
  mutate(proportion = unrate/unrate[series_id == "LNS14000000"],
         increase_unrate = proportion*unrate_increase,
         increase_unrate = proportion*unrate_increase,
         new_unrate = unrate + increase_unrate,
         increased_unemployment = increase_unrate*labor_force) %>%
  select(-series_id, -date, -unrate, -labor_force) %>%
  # Replace "16 years and over" with "all ages"
  mutate(ages_text = recode(ages_text, 
                            "16 years and over" = "all ages")) %>%
  # Create a gt table
  gt() %>%
  tab_header(
    title = "Unemployment Rate Increase by Demographics"
  ) %>%
  cols_label(
    ages_text = "Age Group",
    race_text = "Race",
    sexs_text = "Sex",
    proportion = "Proportion",
    increase_unrate = "Increase in Unrate",
    new_unrate = "New Unrate",
    increased_unemployment = "Increased Unemployment"
  ) %>%
  fmt_number(
    columns = vars(proportion, increase_unrate, new_unrate, increased_unemployment),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F0F0F0"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E5F6E0")
    ),
    locations = cells_body(
      columns = c("proportion", "increase_unrate", "new_unrate")
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FCE1E0")
    ),
    locations = cells_body(
      columns = "increased_unemployment"
    )
  )


  
