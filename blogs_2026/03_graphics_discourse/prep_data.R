# prep_data.R
# Run once to pull BEA NIPA data and save intermediate files for the QMD.
# Requires: tidyusmacro (or govMacroTools), bea.R, tidyverse, lubridate, janitor
# BEA_KEY must be set in .Renviron

library(bea.R)
library(tidyverse)
library(lubridate)
library(janitor)
library(tidyusmacro)

# ── 1. Pull monthly NIPA tables and build PCE inflation dataset ──────────────
nipa <- getNIPAFiles(type = "M")

pce <- getPCEInflation(frequency = "M", NIPA_data = nipa) %>%
  mutate(LineDescription = SeriesLabel, DataValue = Value)

cat("Latest PCE date:", format(max(pce$date), "%B %Y"), "\n")

# ── 2. Save full pce object for downstream use ──────────────────────────────
write_rds(pce, "data/pce_monthly.rds")

# ── 3. Save available line descriptions (useful for exploring what's in pce) ─
line_descriptions <- pce %>%
  distinct(LineDescription) %>%
  arrange(LineDescription)

write_csv(line_descriptions, "data/pce_line_descriptions.csv")
cat("Saved", nrow(line_descriptions), "unique LineDescriptions to data/pce_line_descriptions.csv\n")

# ── 4. Build the inflation contribution table (from 4_inflation_chart.R) ─────

CHECK_ITEMS <- c(
  "PCE services excluding energy",
  "PCE goods excluding food and energy",
  "PCE food and energy",
  "Housing",
  "Personal consumption expenditures"
)

RENAME_ORDER <- c(
  "Personal consumption expenditures" = "1 - Total PCE Inflation",
  "PCE food and energy" = "2 - Food and Energy",
  "PCE goods excluding food and energy" = "3 - Core Goods",
  "Housing" = "5 - Housing"
)

lagged_weight <- function(w, n = 1) {
  out <- dplyr::lag(w, n)
  dplyr::coalesce(out, w)
}

months_between <- function(start_date, end_date) {
  12L * (year(end_date) - year(start_date)) +
    (month(end_date) - month(start_date))
}

period_contrib <- function(x, k_months, annualize = FALSE) {
  lvl_now <- x$DataValue
  lvl_then <- dplyr::lag(x$DataValue, k_months)
  raw_rate <- (lvl_now / lvl_then) - 1
  rate <- if (annualize) (1 + raw_rate)^(12 / k_months) - 1 else raw_rate
  w_tm1 <- lagged_weight(x$PCEweight, 1)
  rate * w_tm1
}

fixed_pair_contrib <- function(df_g, start_date, end_date, annualize = FALSE) {
  v1 <- df_g$DataValue[df_g$date == end_date]
  v0 <- df_g$DataValue[df_g$date == start_date]
  if (length(v1) == 0 || length(v0) == 0) return(NA_real_)
  raw_rate <- (v1 / v0) - 1
  if (annualize) {
    m_span <- months_between(start_date, end_date)
    if (m_span <= 0) return(NA_real_)
    rate <- (1 + raw_rate)^(12 / m_span) - 1
  } else {
    rate <- raw_rate
  }
  w_end <- df_g$PCEweight[df_g$date == end_date]
  w_lag1 <- df_g$PCEweight[df_g$date == (end_date %m-% months(1))]
  w_use <- ifelse(length(w_lag1) == 1, w_lag1, w_end)
  if (length(w_use) == 0) return(NA_real_)
  rate * w_use
}

since_date_contrib <- function(df_g, start_date, annualize = FALSE) {
  end_date <- max(df_g$date, na.rm = TRUE)
  fixed_pair_contrib(df_g, start_date, end_date, annualize = annualize)
}

compute_contribs <- function(pce, categories = CHECK_ITEMS) {
  p <- pce %>%
    filter(LineDescription %in% categories) %>%
    select(date, LineDescription, DataValue, PCEweight) %>%
    arrange(LineDescription, date)

  rolls <- p %>%
    group_by(LineDescription) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      m1_ann = period_contrib(pick(everything()), k_months = 1, annualize = TRUE),
      m3_ann = period_contrib(pick(everything()), k_months = 3, annualize = TRUE),
      m6_ann = period_contrib(pick(everything()), k_months = 6, annualize = TRUE)
    ) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(LineDescription, m1_ann, m3_ann, m6_ann)

  latest_date <- max(p$date, na.rm = TRUE)
  dec_anchor <- as.Date(paste0(year(latest_date) - 1L, "-12-01"))

  anchors <- p %>%
    group_by(LineDescription) %>%
    summarize(
      since_d = since_date_contrib(pick(everything()), dec_anchor, annualize = TRUE),
      yoy_2024 = fixed_pair_contrib(
        pick(everything()), as.Date("2023-12-01"), as.Date("2024-12-01"), annualize = FALSE
      ),
      a_1819 = fixed_pair_contrib(
        pick(everything()), as.Date("2017-12-01"), as.Date("2019-12-01"), annualize = TRUE
      ),
      .groups = "drop"
    )

  rolls %>% left_join(anchors, by = "LineDescription")
}

make_core_non_housing_services <- function(pce) {
  parts <- compute_contribs(
    pce, categories = c("PCE services excluding energy", "Housing")
  ) %>%
    mutate(key = sub("^\\d+\\s*[-–]\\s*", "", LineDescription))

  row_services <- parts %>% filter(str_detect(key, "(?i)^PCE services excluding energy$"))
  row_housing <- parts %>% filter(str_detect(key, "(?i)^Housing$"))

  if (nrow(row_services) != 1 || nrow(row_housing) != 1) return(tibble())

  measure_cols <- c("m1_ann", "m3_ann", "m6_ann", "since_d", "yoy_2024", "a_1819")

  tibble(LineDescription = "4 - Core Non-Housing Services") |>
    bind_cols(
      as_tibble(
        purrr::map_dfc(measure_cols, ~ row_services[[.x]] - row_housing[[.x]])
      ) |> set_names(measure_cols)
    )
}

assemble_table <- function(pce) {
  base <- compute_contribs(pce, categories = CHECK_ITEMS)
  core_nhs <- make_core_non_housing_services(pce)

  cleaned <- base %>%
    filter(!str_detect(LineDescription, "(?i)services excluding energy"))

  latest_date <- max(pce$date, na.rm = TRUE)
  latest_year <- year(latest_date)
  ytd_label <- if (month(latest_date) == 12L) {
    as.character(latest_year)
  } else {
    paste0(latest_year, " (So far)")
  }

  bind_rows(cleaned, core_nhs) %>%
    arrange(LineDescription) %>%
    mutate(LineDescription = sub("^\\d+\\s*[-–]\\s*", "", LineDescription)) %>%
    rename(
      `Past 1 Month` = m1_ann,
      `Past 3 Months` = m3_ann,
      `Past 6 Months` = m6_ann,
      !!ytd_label := since_d,
      `2024` = yoy_2024,
      `2018-2019` = a_1819
    )
}

inflation_table <- assemble_table(pce)
write_csv(inflation_table, "data/inflation_contribution_table.csv")
cat("Saved inflation contribution table to data/inflation_contribution_table.csv\n")

# ── 5. Save core non-housing services time series for the QMD charts ─────────
# Build a monthly time series of the non-housing services price index
# by pulling the two component indexes and constructing the residual contribution.
services_ex_energy <- pce %>%
  filter(LineDescription == "PCE services excluding energy") %>%
  select(date, svc_index = DataValue, svc_weight = PCEweight)

housing <- pce %>%
  filter(LineDescription == "Housing") %>%
  select(date, hsg_index = DataValue, hsg_weight = PCEweight)

nhs_monthly <- services_ex_energy %>%
  inner_join(housing, by = "date") %>%
  arrange(date) %>%
  mutate(
    # YoY inflation rates for each component
    svc_yoy = 100 * (svc_index / lag(svc_index, 12) - 1),
    hsg_yoy = 100 * (hsg_index / lag(hsg_index, 12) - 1),
    # 3-month annualized
    svc_3mo = 100 * ((svc_index / lag(svc_index, 3))^4 - 1),
    hsg_3mo = 100 * ((hsg_index / lag(hsg_index, 3))^4 - 1),
    # 6-month annualized
    svc_6mo = 100 * ((svc_index / lag(svc_index, 6))^2 - 1),
    hsg_6mo = 100 * ((hsg_index / lag(hsg_index, 6))^2 - 1)
  )

write_csv(nhs_monthly, "data/nhs_monthly_components.csv")
cat("Saved non-housing services monthly components to data/nhs_monthly_components.csv\n")

# ── 6. Print all available LineDescriptions matching "market" for exploration ─
market_lines <- pce %>%
  distinct(LineDescription) %>%
  filter(str_detect(LineDescription, "(?i)market")) %>%
  arrange(LineDescription)

cat("\n--- LineDescriptions containing 'market' ---\n")
print(market_lines, n = Inf)

cat("\n--- LineDescriptions containing 'services' ---\n")
services_lines <- pce %>%
  distinct(LineDescription) %>%
  filter(str_detect(LineDescription, "(?i)services")) %>%
  arrange(LineDescription)
print(services_lines, n = Inf)

# ── 7. Build market-based non-housing services time series ────────────────────
# "Market-based PCE services" minus "Market-based PCE housing services"
# gives the Miran-style market-based core non-housing services measure.
mkt_services <- pce %>%
  filter(LineDescription == "Market-based PCE services") %>%
  select(date, mkt_svc_index = DataValue, mkt_svc_weight = PCEweight)

mkt_housing <- pce %>%
  filter(LineDescription == "Market-based PCE housing services") %>%
  select(date, mkt_hsg_index = DataValue, mkt_hsg_weight = PCEweight)

# Also save the overall and market-based totals for comparison
mkt_core <- pce %>%
  filter(LineDescription == "Market-based PCE excluding food and energy") %>%
  select(date, mkt_core_index = DataValue, mkt_core_weight = PCEweight)

mkt_all <- mkt_services %>%
  inner_join(mkt_housing, by = "date") %>%
  left_join(mkt_core, by = "date") %>%
  arrange(date)

write_csv(mkt_all, "data/market_based_components.csv")
cat("Saved market-based service components to data/market_based_components.csv\n")

# ── 8. Build the market-based NHS contribution table row ─────────────────────
# Same approach as make_core_non_housing_services but with market-based lines
mkt_contribs <- compute_contribs(
  pce,
  categories = c("Market-based PCE services", "Market-based PCE housing services")
)

# The compute_contribs function renames via RENAME_ORDER; market-based lines
# won't match, so they keep their original names.
row_mkt_svc <- mkt_contribs %>%
  filter(str_detect(LineDescription, "(?i)^Market-based PCE services$"))
row_mkt_hsg <- mkt_contribs %>%
  filter(str_detect(LineDescription, "(?i)housing"))

if (nrow(row_mkt_svc) == 1 && nrow(row_mkt_hsg) == 1) {
  measure_cols <- c("m1_ann", "m3_ann", "m6_ann", "since_d", "yoy_2024", "a_1819")
  mkt_nhs_row <- tibble(LineDescription = "Market-Based Core Non-Housing Services") |>
    bind_cols(
      as_tibble(
        purrr::map_dfc(measure_cols, ~ row_mkt_svc[[.x]] - row_mkt_hsg[[.x]])
      ) |> set_names(measure_cols)
    )

  # Append to the inflation table
  latest_date <- max(pce$date, na.rm = TRUE)
  latest_year <- year(latest_date)
  ytd_label <- if (month(latest_date) == 12L) {
    as.character(latest_year)
  } else {
    paste0(latest_year, " (So far)")
  }

  mkt_nhs_formatted <- mkt_nhs_row %>%
    rename(
      `Past 1 Month` = m1_ann,
      `Past 3 Months` = m3_ann,
      `Past 6 Months` = m6_ann,
      !!ytd_label := since_d,
      `2024` = yoy_2024,
      `2018-2019` = a_1819
    )

  extended_table <- bind_rows(inflation_table, mkt_nhs_formatted)
  write_csv(extended_table, "data/inflation_contribution_table_extended.csv")
  cat("Saved extended inflation table (with market-based NHS) to data/inflation_contribution_table_extended.csv\n")
}

cat("\nprep_data.R complete.\n")
