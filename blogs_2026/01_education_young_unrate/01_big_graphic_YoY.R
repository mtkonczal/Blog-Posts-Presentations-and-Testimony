# ── 0. Libraries ────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(ipumsr)
library(janitor)
library(zoo)
library(scales)
library(data.table)

# ── 1. Controls ─────────────────────────────────────────────────────────────
MONTHS_WINDOW <- 3
TRAIN_END_YEAR <- 2019
CENTER_AGE_COLLEGE_START <- 22   # College starts at 22 (ages 21-23)
CENTER_AGE_NONCOL_START <- 19    # Non-college starts at 19 (ages 18-20)
CENTER_AGE_END <- 64             # Both end at 64 (ages 63-65)
BAND_WIDTH <- 3

age_band_bounds <- function(center_age, width = 5) {
  half <- (width - 1) / 2
  list(age_min = center_age - half, age_max = center_age + half)
}

# ── 2. Helpers: log-log regression + 12mo MA unemployment from totals ───────
add_unrate_prediction_loglog <- function(
  overall_df,
  subgroup_df,
  overall_col,
  subgroup_col,
  train_end_year = 2019
) {
  stopifnot(is.character(overall_col), length(overall_col) == 1)
  stopifnot(is.character(subgroup_col), length(subgroup_col) == 1)

  pred_col <- paste0(subgroup_col, "_predict")

  regression_data <- overall_df %>%
    select(date, !!rlang::sym(overall_col)) %>%
    left_join(
      subgroup_df %>% select(date, !!rlang::sym(subgroup_col)),
      by = "date"
    ) %>%
    arrange(date) %>%
    filter(.data[[overall_col]] > 0, .data[[subgroup_col]] > 0)

  # Log-log regression

  model_reg <- lm(
    stats::as.formula(paste0("log(", subgroup_col, ") ~ log(", overall_col, ")")),
    data = regression_data %>% filter(lubridate::year(date) <= train_end_year)
  )

  regression_data %>%
    mutate(
      !!rlang::sym(pred_col) := exp(as.numeric(stats::predict(
        model_reg,
        newdata = regression_data
      )))
    )
}

make_unrate_ma12_from_totals <- function(totals_df, out_col) {

  # totals_df must have: date, unemp, lf
  totals_df %>%
    arrange(date) %>%
    mutate(
      urate = unemp / lf,
      urate_ma12 = zoo::rollmean(urate, k = 12, align = "right", fill = NA)
    ) %>%
    filter(!is.na(urate_ma12)) %>%
    transmute(date, !!rlang::sym(out_col) := urate_ma12)
}

# ── 3. Load CPS (trim early) ────────────────────────────────────────────────
cps_raw <- read_ipums_micro(
  "/Users/mtkonczal/Documents/data_folder/cps_00034.xml"
) |>
  clean_names() %>%
  filter(!(year == 2025 & month == 11))

# Keep only what we need
# Education definitions per IPUMS CPS educ codes:
#   educ >= 111: Bachelor's degree or higher
#   educ >= 73 & educ < 111: HS diploma through some college (no BA)
#   educ < 73: Less than HS diploma
cps <- cps_raw %>%
  transmute(
    date = make_date(year, month, 1),
    age = age,
    wtfinl = wtfinl,
    lf_flag = labforce == 2,
    unemp_flag = empstat %in% c(20, 21, 22),

    # Education definitions:
    college = educ >= 111,
    hs_plus = educ >= 73 & educ < 111,
    no_hs = educ < 73
  ) %>%
  filter(age >= 16)

# ── 4. Pre-collapse once: date × age totals for each edu group + overall ────
dt <- as.data.table(cps)

# Weighted components
dt[, unemp_w := wtfinl * unemp_flag]
dt[, lf_w := wtfinl * lf_flag]

# Overall totals by date (for RHS series)
overall_totals_dt <- dt[,
  .(
    unemp = sum(unemp_w, na.rm = TRUE),
    lf = sum(lf_w, na.rm = TRUE)
  ),
  by = .(date)
]
setorder(overall_totals_dt, date)

# Age-by-month totals for each edu group (small table)
age_month_dt <- dt[,
  .(
    unemp_college = sum(unemp_w * college, na.rm = TRUE),
    lf_college = sum(lf_w * college, na.rm = TRUE),
    unemp_hs_plus = sum(unemp_w * hs_plus, na.rm = TRUE),
    lf_hs_plus = sum(lf_w * hs_plus, na.rm = TRUE),
    unemp_no_hs = sum(unemp_w * no_hs, na.rm = TRUE),
    lf_no_hs = sum(lf_w * no_hs, na.rm = TRUE)
  ),
  by = .(date, age)
]
setorder(age_month_dt, date, age)

# Free memory from raw microdata now that we have collapsed tables
rm(cps, cps_raw, dt)
gc()

# Build overall unemployment 12mo MA series once
all_unrate <- make_unrate_ma12_from_totals(
  totals_df = as_tibble(overall_totals_dt),
  out_col = "total_unrate"
)

# ── 5. Compute one center-age residual avg using the collapsed table ─────────
band_totals_dt <- function(
  age_month_dt,
  age_min,
  age_max,
  group = c("college", "hs_plus", "no_hs")
) {
  group <- match.arg(group)

  if (group == "college") {
    age_month_dt[
      age >= age_min & age <= age_max,
      .(
        unemp = sum(unemp_college, na.rm = TRUE),
        lf = sum(lf_college, na.rm = TRUE)
      ),
      by = .(date)
    ][order(date)]
  } else if (group == "hs_plus") {
    age_month_dt[
      age >= age_min & age <= age_max,
      .(
        unemp = sum(unemp_hs_plus, na.rm = TRUE),
        lf = sum(lf_hs_plus, na.rm = TRUE)
      ),
      by = .(date)
    ][order(date)]
  } else {
    age_month_dt[
      age >= age_min & age <= age_max,
      .(
        unemp = sum(unemp_no_hs, na.rm = TRUE),
        lf = sum(lf_no_hs, na.rm = TRUE)
      ),
      by = .(date)
    ][order(date)]
  }
}

compute_one_center_age <- function(
  center_age,
  group,
  age_month_dt,
  all_unrate,
  months_window = 6,
  train_end_year = 2019,
  band_width = 5
) {
  b <- age_band_bounds(center_age, width = band_width)
  age_min <- b$age_min
  age_max <- b$age_max

  totals_dt <- band_totals_dt(age_month_dt, age_min, age_max, group = group)
  subgroup_col <- paste0(group, "_age", age_min, "_", age_max)

  sub_unrate <- make_unrate_ma12_from_totals(
    totals_df = as_tibble(totals_dt),
    out_col = subgroup_col
  )

  with_pred <- add_unrate_prediction_loglog(
    overall_df = all_unrate,
    subgroup_df = sub_unrate,
    overall_col = "total_unrate",
    subgroup_col = subgroup_col,
    train_end_year = train_end_year
  )

  pred_col <- paste0(subgroup_col, "_predict")
  last_block <- with_pred %>% tail(months_window)

  tibble(
    center_age = center_age,
    age_min = age_min,
    age_max = age_max,
    edu_group = group,
    months = nrow(last_block),
    diff_avg = mean(
      last_block[[subgroup_col]] - last_block[[pred_col]],
      na.rm = TRUE
    )
  )
}

# ── 6. Loop one-at-a-time (small objects only) ──────────────────────────────
# Different starting ages for each group
centers_college <- CENTER_AGE_COLLEGE_START:CENTER_AGE_END
centers_noncol <- CENTER_AGE_NONCOL_START:CENTER_AGE_END

out_list <- vector("list", length(centers_college) + 2 * length(centers_noncol))
k <- 1L

# College loop
for (c_age in centers_college) {
  out_list[[k]] <- compute_one_center_age(
    center_age = c_age,
    group = "college",
    age_month_dt = age_month_dt,
    all_unrate = all_unrate,
    months_window = MONTHS_WINDOW,
    train_end_year = TRAIN_END_YEAR,
    band_width = BAND_WIDTH
  )
  k <- k + 1L
}

# HS+ (no BA) loop
for (c_age in centers_noncol) {
  out_list[[k]] <- compute_one_center_age(
    center_age = c_age,
    group = "hs_plus",
    age_month_dt = age_month_dt,
    all_unrate = all_unrate,
    months_window = MONTHS_WINDOW,
    train_end_year = TRAIN_END_YEAR,
    band_width = BAND_WIDTH
  )
  k <- k + 1L
}

# No HS loop
for (c_age in centers_noncol) {
  out_list[[k]] <- compute_one_center_age(
    center_age = c_age,
    group = "no_hs",
    age_month_dt = age_month_dt,
    all_unrate = all_unrate,
    months_window = MONTHS_WINDOW,
    train_end_year = TRAIN_END_YEAR,
    band_width = BAND_WIDTH
  )
  k <- k + 1L
}

age_diff <- bind_rows(out_list) %>%
  mutate(
    edu_group = recode(
      edu_group,
      college = "College+",
      hs_plus = "HS+ (no BA)",
      no_hs = "< HS"
    )
  )

# Save full three-line version
write_csv(age_diff, "data/age_diff_3lines.csv")

# Save two-line version (College+ and HS+ only) for main figure
age_diff_2lines <- age_diff %>%
  filter(edu_group %in% c("College+", "HS+ (no BA)"))
write_csv(age_diff_2lines, "data/age_diff.csv")

# ── 7. Three-line plot (save to file) ───────────────────────────────────────
p_3lines <- ggplot(age_diff, aes(x = center_age, y = diff_avg, color = edu_group)) +
 geom_hline(yintercept = 0, linewidth = 0.6, alpha = 0.6) +
 geom_line(linewidth = 1) +
 geom_point(size = 1.8) +
 scale_color_manual(values = c("College+" = "#1b9e77", "HS+ (no BA)" = "#d95f02", "< HS" = "#7570b3")) +
 scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
 scale_x_continuous(breaks = seq(20, 65, 5), limits = c(19, 65)) +
 labs(
   title = "Actual minus projected unemployment, by age and education",
   subtitle = paste0(
     "Average residual over last ", MONTHS_WINDOW,
     " months; log-log model trained through ", TRAIN_END_YEAR,
     "; College+ starts at 21-23, HS+ and < HS start at 18-20"
   ),
   x = "Center age (band is center ± 1 year)",
   y = "Avg (Actual - Projected) unemployment rate",
   color = NULL,
   caption = "Source: CPS Microdata, Author's Calculations"
 ) +
 theme_minimal(base_size = 12)

ggsave("graphics/age_diff_3lines.png", p_3lines, width = 10, height = 6, dpi = 150)

# ── 8. Two-line plot (for display) ──────────────────────────────────────────
ggplot(age_diff_2lines, aes(x = center_age, y = diff_avg, color = edu_group)) +
 geom_hline(yintercept = 0, linewidth = 0.6, alpha = 0.6) +
 geom_line(linewidth = 1) +
 geom_point(size = 1.8) +
 scale_color_manual(values = c("College+" = "#1b9e77", "HS+ (no BA)" = "#d95f02")) +
 scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
 scale_x_continuous(breaks = seq(20, 65, 5), limits = c(19, 65)) +
 labs(
   title = "Actual minus projected unemployment, by age (3-year bands)",
   subtitle = paste0(
     "Each point is avg(actual - predicted) over last ",
     MONTHS_WINDOW,
     " months; log-log prediction trained through ",
     TRAIN_END_YEAR,
     "; College+ starts at 21-23, HS+ starts at 18-20"
   ),
   x = "Center age (band is center ± 1 year)",
   y = "Avg (Actual - Projected) unemployment rate",
   color = NULL
 ) +
 theme_minimal(base_size = 12)
