library(tidyverse)
library(httr2)
library(jsonlite)
library(scales)
library(patchwork)

eia_key <- Sys.getenv("EIA_KEY")

# --- helper: build EIA v2 URL manually (bracket params don't play nice with httr2) ---
pull_eia <- function(route, series_id, name, freq = "weekly") {
  base <- paste0("https://api.eia.gov/v2/", route, "/data/")
  url <- paste0(
    base,
    "?api_key=",
    eia_key,
    "&frequency=",
    freq,
    "&data[0]=value",
    "&facets[series][]=",
    series_id,
    "&sort[0][column]=period",
    "&sort[0][direction]=asc",
    "&offset=0",
    "&length=5000"
  )

  resp <- request(url) |> req_perform()
  body <- resp |> resp_body_json()

  # paginate if needed
  total <- as.integer(body$response$total)
  all_data <- body$response$data

  if (total > 5000) {
    pages <- ceiling(total / 5000)
    for (p in 2:pages) {
      off <- (p - 1) * 5000
      url_p <- sub("&offset=0", paste0("&offset=", off), url)
      resp_p <- request(url_p) |> req_perform()
      body_p <- resp_p |> resp_body_json()
      all_data <- c(all_data, body_p$response$data)
    }
  }

  tibble(
    date = as.Date(map_chr(all_data, "period")),
    value = as.numeric(map_chr(all_data, "value", .default = NA))
  ) |>
    filter(!is.na(value)) |>
    set_names("date", name)
}

# --- pull WTI spot ($/barrel, weekly) ---
cat("Pulling WTI spot prices...\n")
wti <- pull_eia("petroleum/pri/spt", "RWTC", "wti")
cat(
  "  Got",
  nrow(wti),
  "obs:",
  as.character(min(wti$date)),
  "to",
  as.character(max(wti$date)),
  "\n"
)

# --- pull U.S. regular gasoline retail ($/gallon, weekly) ---
cat("Pulling retail gasoline prices...\n")
gas <- pull_eia("petroleum/pri/gnd", "EMM_EPMR_PTE_NUS_DPG", "gas")
cat(
  "  Got",
  nrow(gas),
  "obs:",
  as.character(min(gas$date)),
  "to",
  as.character(max(gas$date)),
  "\n"
)

# --- merge: shift Friday WTI forward 3 days to match the following Monday's gas price ---
wti <- wti |> mutate(monday = date + 3)

df <- inner_join(
  wti |> select(monday, wti),
  gas |> rename(monday = date),
  by = "monday"
) |>
  arrange(monday) |>
  rename(date = monday)
cat(
  "\nMerged dataset:",
  nrow(df),
  "weekly obs from",
  as.character(min(df$date)),
  "to",
  as.character(max(df$date)),
  "\n\n"
)

# =========================================================================
# 1. DUAL-AXIS TIME SERIES PLOT
# =========================================================================
wti_min <- min(df$wti)
wti_max <- max(df$wti)
gas_min <- min(df$gas)
gas_max <- max(df$gas)
a <- (gas_max - gas_min) / (wti_max - wti_min)
b <- gas_min - a * wti_min

p1 <- ggplot(df, aes(date)) +
  geom_line(aes(y = gas), color = "#e63946", linewidth = 0.4) +
  geom_line(
    aes(y = a * wti + b),
    color = "#457b9d",
    linewidth = 0.4,
    alpha = 0.8
  ) +
  scale_y_continuous(
    name = "Retail gasoline ($/gal)",
    labels = dollar,
    sec.axis = sec_axis(
      ~ (. - b) / a,
      name = "WTI crude ($/barrel)",
      labels = dollar
    )
  ) +
  labs(
    title = "WTI Crude Oil vs U.S. Retail Gasoline (Weekly)",
    subtitle = "Red = gasoline · Blue = WTI",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y.left = element_text(color = "#e63946"),
    axis.title.y.right = element_text(color = "#457b9d")
  )

# =========================================================================
# 2. CROSS-CORRELATION of weekly changes
# =========================================================================
df_chg <- df |>
  mutate(
    d_wti = wti - lag(wti),
    d_gas = gas - lag(gas),
    d_wti_pos = if_else(d_wti > 0, d_wti, 0),
    d_wti_neg = if_else(d_wti < 0, d_wti, 0)
  ) |>
  filter(!is.na(d_wti), !is.na(d_gas))

max_lag <- 12
ccf_vals <- ccf(df_chg$d_wti, df_chg$d_gas, lag.max = max_lag, plot = FALSE)

ccf_df <- tibble(
  lag = as.integer(ccf_vals$lag),
  corr = as.numeric(ccf_vals$acf)
) |>
  filter(lag >= 0)

p2 <- ggplot(ccf_df, aes(lag, corr)) +
  geom_col(fill = "#457b9d", width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_hline(
    yintercept = c(-1, 1) * qnorm(0.975) / sqrt(nrow(df_chg)),
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_x_continuous(breaks = 0:max_lag) +
  labs(
    title = "Cross-Correlation: Weekly ΔWTI → Weekly ΔGas",
    subtitle = "Lag in weeks (dashed = 95% significance bounds)",
    x = "Lag (weeks)",
    y = "Correlation"
  ) +
  theme_minimal(base_size = 12)

# =========================================================================
# 3. DISTRIBUTED LAG REGRESSION
# =========================================================================
n_lags <- 6

lag_mat <- map_dfc(0:n_lags, \(k) {
  tibble(!!paste0("L", k) := lag(df_chg$d_wti, k))
})

reg_df <- bind_cols(tibble(d_gas = df_chg$d_gas), lag_mat) |> drop_na()

fit <- lm(d_gas ~ . - 1, data = reg_df)
cat(
  "--- Distributed Lag Regression: ΔGas ~ ΔWTIₜ + ΔWTIₜ₋₁ + ... + ΔWTIₜ₋₆ ---\n"
)
print(summary(fit))

coefs <- tibble(
  lag = 0:n_lags,
  coef = coef(fit) * 50,
  se = summary(fit)$coefficients[, 2] * 10
) |>
  mutate(cumulative = cumsum(coef))

cat("\nCumulative pass-through by week (per $50/barrel WTI increase):\n")
print(
  coefs |>
    select(lag, coef, cumulative) |>
    mutate(across(c(coef, cumulative), \(x) round(x, 4)))
)
cat(
  "\nInterpretation: a $10/barrel WTI increase leads to ~$",
  round(sum(coefs$coef), 3),
  "/gallon increase over",
  n_lags,
  "weeks.\n"
)
cat(
  "Rule-of-thumb check: 10/42 = $",
  round(10 / 42, 4),
  "/gallon (pure crude input).\n"
)

p3 <- ggplot(coefs, aes(lag, cumulative)) +
  geom_col(aes(y = coef), fill = "#a8dadc", width = 0.6) +
  geom_text(
    aes(y = coef, label = sprintf("%.2f", coef)),
    vjust = -0.5,
    size = 3
  ) +
  geom_point(color = "#e63946", size = 2.5) +
  geom_line(color = "#e63946", linewidth = 1) +
  scale_x_continuous(breaks = 0:n_lags) +
  labs(
    title = "Lagged Pass-Through to Gas Prices per $50/barrel WTI increase",
    subtitle = "Bars = marginal effect each week · Line = cumulative, training data EIA 1990 - 2026",
    x = "Lag (weeks)",
    y = "$/gallon per $10/barrel WTI",
    caption = "EIA, Mike Konczal"
  ) +
  theme_minimal(base_size = 15) +
  scale_y_continuous(labels = dollar)
theme(plot.title.position = "plot")

# =========================================================================
# 4. ROCKETS & FEATHERS: asymmetric pass-through
# =========================================================================
asym_df <- reg_df |>
  mutate(direction = if_else(L0 >= 0, "up", "down"))

fit_up <- lm(
  d_gas ~ L0 + L1 + L2 + L3 + L4 + L5 + L6 - 1,
  data = filter(asym_df, direction == "up")
)
fit_down <- lm(
  d_gas ~ L0 + L1 + L2 + L3 + L4 + L5 + L6 - 1,
  data = filter(asym_df, direction == "down")
)

asym_coefs <- bind_rows(
  tibble(lag = 0:n_lags, coef = coef(fit_up), direction = "Crude rising"),
  tibble(lag = 0:n_lags, coef = coef(fit_down), direction = "Crude falling")
) |>
  group_by(direction) |>
  mutate(cumulative = cumsum(coef)) |>
  ungroup()

cat("\n--- Rockets & Feathers ---\n")
cat(
  "Total pass-through when crude rises: $",
  round(sum(coef(fit_up)), 4),
  "/gal per $1/bbl\n"
)
cat(
  "Total pass-through when crude falls: $",
  round(sum(coef(fit_down)), 4),
  "/gal per $1/bbl\n"
)

p4 <- ggplot(asym_coefs, aes(lag, cumulative, color = direction)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Crude rising" = "#e63946", "Crude falling" = "#457b9d")
  ) +
  scale_x_continuous(breaks = 0:n_lags) +
  labs(
    title = "Rockets & Feathers: Asymmetric Pass-Through",
    subtitle = "Cumulative response split by WTI increase vs decrease weeks",
    x = "Lag (weeks)",
    y = "Cumulative $/gal per $1/bbl",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# =========================================================================
# COMBINE AND DISPLAY
# =========================================================================
# =========================================================================
# 5. ASYMMETRIC DISTRIBUTED LAG — rising vs falling in a single model
# =========================================================================
lag_mat_pos <- map_dfc(0:n_lags, \(k) {
  tibble(!!paste0("L", k, "_pos") := lag(df_chg$d_wti_pos, k))
})
lag_mat_neg <- map_dfc(0:n_lags, \(k) {
  tibble(!!paste0("L", k, "_neg") := lag(df_chg$d_wti_neg, k))
})

reg_df_asym <- bind_cols(
  tibble(d_gas = df_chg$d_gas),
  lag_mat_pos,
  lag_mat_neg
) |>
  drop_na()

fit_asym <- lm(d_gas ~ . - 1, data = reg_df_asym)
cat("\n--- Asymmetric Distributed Lag Model ---\n")
print(summary(fit_asym))

coefs_p5 <- bind_rows(
  tibble(
    lag = 0:n_lags,
    coef = coef(fit_asym)[paste0("L", 0:n_lags, "_pos")] * 10,
    group = "Crude rising"
  ),
  tibble(
    lag = 0:n_lags,
    coef = coef(fit_asym)[paste0("L", 0:n_lags, "_neg")] * 10,
    group = "Crude falling"
  )
) |>
  group_by(group) |>
  mutate(cumulative = cumsum(coef)) |>
  ungroup()

cat("\nCumulative pass-through per $10/bbl (asymmetric model):\n")
print(
  coefs_p5 |>
    select(group, lag, coef, cumulative) |>
    mutate(across(c(coef, cumulative), \(x) round(x, 4)))
)

p5 <- ggplot(coefs_p5, aes(lag, color = group)) +
  geom_col(
    aes(y = coef, fill = group),
    position = position_dodge(width = 0.7),
    width = 0.6,
    alpha = 0.35
  ) +
  geom_text(
    aes(y = coef, label = sprintf("%.2f", coef), group = group),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 2.8
  ) +
  geom_line(aes(y = cumulative, linetype = group), linewidth = 1) +
  geom_point(aes(y = cumulative), size = 2.5) +
  scale_color_manual(
    values = c("Crude rising" = "#e76f51", "Crude falling" = "#457b9d")
  ) +
  scale_fill_manual(
    values = c("Crude rising" = "#e76f51", "Crude falling" = "#457b9d")
  ) +
  scale_linetype_manual(
    values = c("Crude rising" = "solid", "Crude falling" = "dashed")
  ) +
  scale_x_continuous(breaks = 0:n_lags) +
  labs(
    title = "Rockets & Feathers: $10/bbl WTI Move — Rising vs Falling",
    subtitle = "Bars = marginal effect each week · Lines = cumulative (single asymmetric model)",
    x = "Lag (weeks)",
    y = "$/gallon per $10/barrel WTI",
    color = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# =========================================================================
# COMBINE AND DISPLAY
# =========================================================================
combined <- (p1 / p2) | (p3 / p4)
combined +
  plot_annotation(
    title = "WTI Crude → U.S. Retail Gasoline: Price Pass-Through Analysis",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

p5
