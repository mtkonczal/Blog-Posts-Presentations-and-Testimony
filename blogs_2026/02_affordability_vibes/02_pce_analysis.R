library(tidyverse)

pce <- read_csv("data/pce_data.csv") %>%
  distinct(date, item_name, .keep_all = TRUE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01")) %>%
  arrange(item_name, date)

core_essential_items <- c(
  "Housing and utilities",
  "Food and beverages purchased for off-premises consumption"
)

essential_items <- c(
  core_essential_items,
  "Health care",
  "Transportation services",
  "Motor vehicles and parts"
)

build_contrib_index <- function(items, label) {
  # 1) restrict to bundle and renormalize weights within bundle each date
  df <- pce %>%
    filter(item_name %in% items) %>%
    group_by(date) %>%
    mutate(share = PCEweight / sum(PCEweight, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(item_name, date) %>%
    group_by(item_name) %>%
    mutate(
      dln_p = log(value) - log(lag(value)), # one-month log inflation
      share_lag = lag(share),
      avg_share = (share + share_lag) / 2, # Törnqvist avg share
      contrib = avg_share * dln_p # additive contribution (log points)
    ) %>%
    ungroup()

  # 2) aggregate additive contributions to bundle inflation each month
  bundle <- df %>%
    group_by(date) %>%
    summarize(
      dln_index = sum(contrib, na.rm = TRUE), # bundle log inflation
      .groups = "drop"
    ) %>%
    arrange(date) %>%
    mutate(
      index_raw = exp(cumsum(replace_na(dln_index, 0))), # chain to level (starts at 1)
      index_type = label
    )

  bundle
}

core_idx <- build_contrib_index(core_essential_items, "Core Essentials")
full_idx <- build_contrib_index(essential_items, "Essentials")

overall_pce <- pce %>%
  filter(item_name == "Personal consumption expenditures") %>%
  transmute(date, index_raw = value, index_type = "Overall PCE")

all_indices <- bind_rows(core_idx, full_idx, overall_pce)

# Normalize each series to Dec 2019 = 100
base_date <- as.Date("2019-12-01")

all_indices <- all_indices %>%
  group_by(index_type) %>%
  mutate(
    base = index_raw[date == base_date][1],
    index_norm = index_raw / base * 100
  ) %>%
  ungroup() %>%
  select(date, index_type, index_norm)

write_csv(all_indices, "data/pce_essentials_indices.csv")
cat("Saved to data/pce_essentials_indices.csv\n")
