# ── 0.  Libraries ───────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(ipumsr)
library(janitor)
library(haven)      # for as_factor() if you need labelled → factor later

# ── 1.  Read the CPS Basic-Monthly extract (2015, 2019, 2024) ───────────────
cps <- read_ipums_micro("/Users/mtkonczal/Documents/data_folder/cps_00003.xml") |>
  clean_names()

# ── 2.  Core labour-force flags ─────────────────────────────────────────────
# • LABFORCE == 2  → in the labour force (employed **or** unemployed)
# • EMPSTAT  codes
#     10  employed − at work
#     12  employed − absent
#     20, 21, 22  unemployed (20 appears only in pre-1994 data)
cps <- cps |>
  filter(year %in% c(2015,2019,2024)) %>%
  mutate(
    year_month = make_date(year, month, 1),
    lf_flag    = labforce == 2,
    unemp_flag = empstat %in% c(20, 21, 22)
  )

# ── 3.  Build analysis groups ───────────────────────────────────────────────
cps_groups <- cps |>
  filter(age >= 16) |>                     # BLS convention
  mutate(
    group = case_when(
      age %in% 22:27 & educ >  111 ~ "22-27 BA+",
      age %in% 22:27 & educ == 111 ~ "22-27 BA",
      age %in% 22:27               ~ "22-27 all",
      TRUE                         ~ "All (16+)"
    )
  )

# ── 4.  Monthly unemployment rate by group ──────────────────────────────────
ur_month <- cps_groups |>
  group_by(group, year_month, sex) |>
  summarise(
    unemp = sum(wtfinl * unemp_flag, na.rm = TRUE),
    lf    = sum(wtfinl * lf_flag,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(urate = unemp / lf) %>%
  ungroup()

# ── 5.  Annual average unemployment rate (percent, 1 dp) ────────────────────
ur_annual <- ur_month |>
  mutate(year = year(year_month)) |>
  group_by(group, year, sex) |>
  summarise(
    unemployment_rate = round(mean(urate) * 100, 1),  # convert to %
    .groups = "drop"
  ) |>
  arrange(group, year, sex) %>%
  ungroup()

print(ur_annual, n = Inf)
