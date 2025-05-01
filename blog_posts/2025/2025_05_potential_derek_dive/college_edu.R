# ── 0.  Libraries ────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(ipumsr)
library(janitor)
library(haven)      # labelled → factor

# ── 1.  Read the CPS Basic-Monthly extract (2015, 2019, 2024) ────────────────
cps <- read_ipums_micro("/Users/mtkonczal/Documents/data_folder/cps_00003.xml") %>% 
  clean_names()

# ── 2.  Prep: keep cohort & two target years ────────────────────────────────
cps_small <- cps %>% 
  mutate(year_month = make_date(year, month, 1)) %>% 
  filter(year %in% c(2019, 2024),           # comparison years
         age  %in% 22:27,                  # young workers
         educ >= 111)                      # BA+ (111 = bachelor's)
# NOTE: wtfinl is already on the correct scale — do NOT rescale or as.numeric()

# ── 3.  Helper to build the summary for any categorical var ─────────────────
make_change_table <- function(df, cat_var, wt = wtfinl) {
  
  # 3a. employment totals by month and category
  by_mo <- df %>% 
    group_by({{ cat_var }}, year_month, year) %>% 
    summarise(emp = sum({{ wt }}, na.rm = TRUE), .groups = "drop")
  
  # 3b. average across the 12 months in each year
  by_yr <- by_mo %>% 
    group_by({{ cat_var }}, year) %>% 
    summarise(emp = mean(emp), .groups = "drop")
  
  # 3c. reshape, compute change
  out <- by_yr %>% 
    pivot_wider(names_from = year, values_from = emp, names_prefix = "y") %>% 
    mutate(change      = y2024 - y2019,
           pct_change  = 100 * change / y2019) %>% 
    filter(change < 0)                       # **negative growth only**
  
  out
}

# ── 4.  Occupations (OCC) ───────────────────────────────────────────────────
#       OCC == 0 is "Not in Universe"; drop it up front.
occ_neg <- cps_small %>% 
  filter(occ != 0) %>% 
  # turn labelled integer codes into text so the table is readable
  mutate(ind_lbl = as_factor(occ2010, levels = "labels")) %>% 
  make_change_table(ind_lbl)  %>%
  mutate(type = "Occupation")

# ── 5.  Industries (IND) ────────────────────────────────────────────────────
#       IND == 0 is "Not in Universe"; drop it too.
ind_neg <- cps_small %>% 
  filter(ind != 0) %>% 
  mutate(ind_lbl = as_factor(ind1990, levels = "labels")) %>% 
  make_change_table(ind_lbl) %>%
  mutate(type = "Industry")

round_tbl <- function(df) {
  df %>% 
    mutate(
      across(c(y2019, y2024, change), ~ round(.x)),   
      pct_change = round(pct_change, 1)                   # 0.1 pp
    )
}

occ_neg  <- occ_neg  %>% round_tbl()
ind_neg  <- ind_neg  %>% round_tbl()

# ── 6.  Inspect results ─────────────────────────────────────────────────────
occ_neg %>% arrange(pct_change) %>% print(n = 20)   # worst-hit occupations
ind_neg %>% arrange(pct_change) %>% print(n = 20)   # worst-hit industries

write_csv(rbind(occ_neg, ind_neg) %>% arrange(change), "output/negative_growth_22_27_BA_plus.csv")
