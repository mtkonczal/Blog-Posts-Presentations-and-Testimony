# load_data.R
library(tidycensus)
library(tidyverse)

# --- 1. Define Variables ---
# Note: use base ACS codes (no trailing "E").
vars <- c(
  rent = "B25064_001", # Median Gross Rent
  hh_income = "B19013_001", # Median Household Income
  total_pop = "B01003_001", # Total Population
  foreign_born = "B05002_013" # Foreign Born Population
)

# --- 2. Helper Function to Fetch Data for One Year ---
fetch_year <- function(yr) {
  message(paste("Fetching ACS 1-year data for:", yr))

  get_acs(
    geography = "cbsa",
    year = yr,
    survey = "acs1",
    variables = vars,
    output = "wide"
  ) %>%
    # keep NAME and GEOID
    # keep only estimate columns: rentE, hh_incomeE, etc.
    select(GEOID, NAME, ends_with("E")) %>%
    # strip "E" so rentE -> rent, hh_incomeE -> hh_income
    rename_with(~ sub("E$", "", .x), -c(GEOID, NAME)) %>%
    # add year prefix: rent -> y2024_rent, etc.
    rename_with(~ paste0("y", yr, "_", .x), -c(GEOID, NAME))
}

# --- 3. Download All Snapshots ---
df_2010 <- fetch_year(2010)
df_2014 <- fetch_year(2014)
df_2019 <- fetch_year(2019)
df_2024 <- fetch_year(2024)

# --- 4. Helper: Build 5-Year Changes Between Two Snapshots ---
create_period <- function(data_start, data_end, start_year, end_year) {
  rent_start <- paste0("y", start_year, "_rent")
  rent_end <- paste0("y", end_year, "_rent")

  inc_start <- paste0("y", start_year, "_hh_income")
  inc_end <- paste0("y", end_year, "_hh_income")

  pop_start <- paste0("y", start_year, "_total_pop")
  pop_end <- paste0("y", end_year, "_total_pop")

  fb_start <- paste0("y", start_year, "_foreign_born")
  fb_end <- paste0("y", end_year, "_foreign_born")

  data_start %>%
    # join on GEOID and NAME so names are consistent over time
    inner_join(data_end, by = c("GEOID", "NAME")) %>%
    mutate(
      period = paste0(start_year, "-", end_year),

      # Log changes in rent, income, population
      delta_rent = log(.data[[rent_end]]) - log(.data[[rent_start]]),
      delta_income = log(.data[[inc_end]]) - log(.data[[inc_start]]),
      delta_pop = log(.data[[pop_end]]) - log(.data[[pop_start]]),

      # Saiz-style immigrant inflow: Δ foreign-born / initial total population
      immigrant_inflow = (.data[[fb_end]] - .data[[fb_start]]) /
        .data[[pop_start]],

      # Log change in foreign-born
      delta_foreign_log = log(.data[[fb_end]]) - log(.data[[fb_start]]),

      # End-of-period population weight
      pop_weight = .data[[pop_end]]
    ) %>%
    select(
      GEOID,
      NAME,
      period,
      delta_rent,
      delta_income,
      delta_pop,
      immigrant_inflow,
      delta_foreign_log,
      pop_weight
    ) %>%
    filter(
      is.finite(delta_rent),
      is.finite(immigrant_inflow),
      is.finite(delta_foreign_log)
    )
}

# --- 5. Build Full 5-Year Panel in One Data Frame ---
panel_2010s <- bind_rows(
  create_period(df_2010, df_2014, 2010, 2014),
  create_period(df_2014, df_2019, 2014, 2019),
  create_period(df_2019, df_2024, 2019, 2024)
)

# Optionally save
write_rds(panel_2010s, "data_immigration_rent_panel_2010s.rds")
message("Done! Saved 'data_immigration_rent_panel_2010s.rds'")
