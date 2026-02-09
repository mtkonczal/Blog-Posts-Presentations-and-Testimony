# 03_aids_estimation.R
# Almost Ideal Demand System (LA/AIDS) for PCE essentials
# Decomposes budget-share changes into price vs expenditure effects

library(tidyverse)
library(systemfit)

# ---------- 1. Load PCE data ----------
pce <- read_csv("data/pce_data.csv") %>%
  distinct(date, item_name, .keep_all = TRUE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01")) %>%
  arrange(item_name, date)

# ---------- 2. Define goods ----------
core_essential_items <- c(
  "Housing and utilities",
  "Food and beverages purchased for off-premises consumption"
)

other_essential_items <- c(
  "Health care",
  "Transportation services",
  "Motor vehicles and parts"
)

all_essential_items <- c(core_essential_items, other_essential_items)

# ---------- 3. Extract component data ----------
# Get the 5 essential items + food services + overall PCE
items_needed <- c(all_essential_items,
                  "Food services and accommodations",
                  "Personal consumption expenditures")

components <- pce %>%
  filter(item_name %in% items_needed) %>%
  select(date, item_name, value, PCEweight, quantity)

# Pivot to wide format
wide <- components %>%
  pivot_wider(
    id_cols = date,
    names_from = item_name,
    values_from = c(value, PCEweight, quantity)
  ) %>%
  arrange(date) %>%
  drop_na()

# Clean column names for easier access
names(wide) <- names(wide) %>%
  str_replace_all("Food and beverages purchased for off-premises consumption", "food") %>%
  str_replace_all("Food services and accommodations", "foodsvc") %>%
  str_replace_all("Housing and utilities", "housing") %>%
  str_replace_all("Health care", "health") %>%
  str_replace_all("Transportation services", "transport") %>%
  str_replace_all("Motor vehicles and parts", "motor") %>%
  str_replace_all("Personal consumption expenditures", "pce")

# ---------- 4. Construct 3-good aggregate shares ----------
wide <- wide %>%
  mutate(
    w_core  = PCEweight_housing + PCEweight_food,
    w_other = PCEweight_health + PCEweight_transport + PCEweight_motor,
    w_resid = 1 - w_core - w_other
  )

# ---------- 5. Build Törnqvist price indices for core and other bundles ----------
build_tornqvist <- function(df, price_cols, weight_cols) {
  # Renormalize weights within bundle
  w_mat <- as.matrix(df[, weight_cols])
  w_norm <- w_mat / rowSums(w_mat)

  p_mat <- as.matrix(df[, price_cols])

  # Log price changes
  dlnp <- log(p_mat[-1, ]) - log(p_mat[-nrow(p_mat), ])

  # Average renormalized shares (Törnqvist)
  w_avg <- (w_norm[-1, ] + w_norm[-nrow(w_norm), ]) / 2

  # Weighted log-change
  dln_index <- rowSums(dlnp * w_avg)

  # Cumulate from index = 1
  ln_index <- c(0, cumsum(dln_index))
  exp(ln_index)
}

p_core <- build_tornqvist(
  wide,
  c("value_housing", "value_food"),
  c("PCEweight_housing", "PCEweight_food")
)

p_other <- build_tornqvist(
  wide,
  c("value_health", "value_transport", "value_motor"),
  c("PCEweight_health", "PCEweight_transport", "PCEweight_motor")
)

wide <- wide %>%
  mutate(
    p_core  = p_core,
    p_other = p_other,
    p_overall = value_pce / value_pce[1]  # normalize overall PCE price index
  )

# ---------- 6. Residual price via Stone index ----------
# ln(P_overall) = w_core*ln(p_core) + w_other*ln(p_other) + w_resid*ln(p_resid)
# => ln(p_resid) = (ln(p_overall) - w_core*ln(p_core) - w_other*ln(p_other)) / w_resid
wide <- wide %>%
  mutate(
    ln_p_core  = log(p_core),
    ln_p_other = log(p_other),
    ln_p_overall = log(p_overall),
    ln_p_resid = (ln_p_overall - w_core * ln_p_core - w_other * ln_p_other) / w_resid,
    p_resid = exp(ln_p_resid)
  )

# ---------- 7. Construct AIDS variables ----------
wide <- wide %>%
  mutate(
    # Relative log prices (homogeneity imposed)
    lnp1_rel = ln_p_core - ln_p_resid,
    lnp2_rel = ln_p_other - ln_p_resid,

    # Stone price index
    ln_Pstar = w_core * ln_p_core + w_other * ln_p_other + w_resid * ln_p_resid,

    # Nominal expenditure proxy (price * quantity for overall PCE)
    X = value_pce * quantity_pce,
    lnX = log(X),

    # Real expenditure term
    lnXPstar = lnX - ln_Pstar,

    # Time trend (months from start)
    trend = as.numeric(difftime(date, min(date), units = "days")) / 30.44
  )

# ---------- 8. Estimate SUR with symmetry restriction ----------
eq_core  <- w_core  ~ lnp1_rel + lnp2_rel + lnXPstar + trend
eq_other <- w_other ~ lnp1_rel + lnp2_rel + lnXPstar + trend

system_eqs <- list(core = eq_core, other = eq_other)

# Symmetry restriction: gamma_12 = gamma_21
# In equation "core", lnp2_rel is the 3rd coefficient (intercept=1, lnp1_rel=2, lnp2_rel=3)
# In equation "other", lnp1_rel is the 2nd coefficient
restriction <- "core_lnp2_rel - other_lnp1_rel = 0"

aids_data <- wide %>%
  select(w_core, w_other, lnp1_rel, lnp2_rel, lnXPstar, trend) %>%
  drop_na()

sur_model <- systemfit(system_eqs, method = "SUR", data = aids_data,
                       restrict.matrix = restriction)

cat("\n===== AIDS SUR Estimation Results =====\n")
summary(sur_model)

# ---------- 9. Extract coefficients ----------
coefs <- summary(sur_model)$coefficients
coef_df <- tibble(
  term = rownames(coefs),
  estimate = coefs[, "Estimate"],
  std_error = coefs[, "Std. Error"],
  t_value = coefs[, "t value"],
  p_value = coefs[, "Pr(>|t|)"]
)

# ---------- 10. Compute elasticities at sample means ----------
w_bar <- aids_data %>%
  summarize(
    w_core  = mean(w_core),
    w_other = mean(w_other),
    w_resid = 1 - mean(w_core) - mean(w_other)
  )

# Extract key parameters
beta_core  <- coef(sur_model)["core_lnXPstar"]
beta_other <- coef(sur_model)["other_lnXPstar"]
gamma_11   <- coef(sur_model)["core_lnp1_rel"]
gamma_22   <- coef(sur_model)["other_lnp2_rel"]
gamma_12   <- coef(sur_model)["core_lnp2_rel"]  # = gamma_21 by restriction

# Expenditure elasticities
e_exp_core  <- 1 + beta_core  / w_bar$w_core
e_exp_other <- 1 + beta_other / w_bar$w_other

# Residual expenditure elasticity (from adding-up)
beta_resid <- -beta_core - beta_other
e_exp_resid <- 1 + beta_resid / w_bar$w_resid

# Marshallian own-price elasticities
e_M_core  <- -1 + gamma_11 / w_bar$w_core  - beta_core
e_M_other <- -1 + gamma_22 / w_bar$w_other - beta_other

# Residual own-price: gamma_resid_resid from adding-up
gamma_13 <- -gamma_11 - gamma_12
gamma_23 <- -gamma_12 - gamma_22  # using symmetry gamma_21 = gamma_12
gamma_33 <- -gamma_13 - gamma_23
e_M_resid <- -1 + gamma_33 / w_bar$w_resid - beta_resid

# Hicksian own-price elasticities
e_H_core  <- e_M_core  + w_bar$w_core  * e_exp_core
e_H_other <- e_M_other + w_bar$w_other * e_exp_other
e_H_resid <- e_M_resid + w_bar$w_resid * e_exp_resid

# Classify
classify <- function(e) {
  case_when(
    e < 0  ~ "Inferior",
    e < 1  ~ "Necessity",
    e == 1 ~ "Unitary",
    TRUE   ~ "Luxury"
  )
}

elasticities <- tibble(
  good = c("Core Essentials", "Other Essentials", "Everything Else"),
  mean_share = c(w_bar$w_core, w_bar$w_other, w_bar$w_resid),
  expenditure_elasticity = c(e_exp_core, e_exp_other, e_exp_resid),
  marshallian_own_price = c(e_M_core, e_M_other, e_M_resid),
  hicksian_own_price = c(e_H_core, e_H_other, e_H_resid),
  classification = classify(c(e_exp_core, e_exp_other, e_exp_resid))
)

cat("\n===== Elasticities at Sample Means =====\n")
print(elasticities)

# ---------- 11. Save results ----------
write_csv(coef_df, "data/aids_coefficients.csv")
write_csv(elasticities, "data/aids_elasticities.csv")

cat("\nSaved data/aids_coefficients.csv and data/aids_elasticities.csv\n")

# ==========================================================================
# MODEL 2: Disaggregated 5-good AIDS (4 estimated equations + residual)
# Housing, Food at Home, Food Services, Health Care,
# Everything Else
# ==========================================================================
cat("

===== Disaggregated 5-Good AIDS =====
")

# ---------- 12. Individual good shares ----------
wide_disagg <- wide %>%
  mutate(
    w_housing   = PCEweight_housing,
    w_food      = PCEweight_food,
    w_foodsvc   = PCEweight_foodsvc,
    w_health    = PCEweight_health,
    w_resid5    = 1 - w_housing - w_food - w_foodsvc - w_health
  )

# ---------- 13. Individual price indices (normalized to first obs = 1) ----------
wide_disagg <- wide_disagg %>%
  mutate(
    p_housing   = value_housing / value_housing[1],
    p_food      = value_food / value_food[1],
    p_foodsvc   = value_foodsvc / value_foodsvc[1],
    p_health    = value_health / value_health[1]
  )

# ---------- 14. Residual price from Stone index ----------
wide_disagg <- wide_disagg %>%
  mutate(
    ln_p_housing   = log(p_housing),
    ln_p_food      = log(p_food),
    ln_p_foodsvc   = log(p_foodsvc),
    ln_p_health    = log(p_health),
    ln_p_resid5 = (ln_p_overall
                   - w_housing * ln_p_housing
                   - w_food * ln_p_food
                   - w_foodsvc * ln_p_foodsvc
                   - w_health * ln_p_health) / w_resid5,
    p_resid5 = exp(ln_p_resid5)
  )

# ---------- 15. Construct AIDS variables for 5-good model ----------
wide_disagg <- wide_disagg %>%
  mutate(
    # Relative log prices (homogeneity imposed by dividing by p_resid5)
    lnp_h = ln_p_housing - ln_p_resid5,
    lnp_f = ln_p_food - ln_p_resid5,
    lnp_s = ln_p_foodsvc - ln_p_resid5,
    lnp_c = ln_p_health - ln_p_resid5,

    # Stone price index
    ln_Pstar5 = w_housing * ln_p_housing + w_food * ln_p_food +
      w_foodsvc * ln_p_foodsvc + w_health * ln_p_health +
      w_resid5 * ln_p_resid5,

    # Real expenditure
    lnXP5 = lnX - ln_Pstar5
  )

# ---------- 16. Estimate 4-equation SUR with symmetry ----------
price_rhs <- "lnp_h + lnp_f + lnp_s + lnp_c + lnXP5 + trend"

eq_h <- as.formula(paste("w_housing ~",   price_rhs))
eq_f <- as.formula(paste("w_food ~",      price_rhs))
eq_s <- as.formula(paste("w_foodsvc ~",   price_rhs))
eq_c <- as.formula(paste("w_health ~",    price_rhs))

system5 <- list(housing = eq_h, food = eq_f, foodsvc = eq_s,
                health = eq_c)

# Symmetry: gamma_ij = gamma_ji for all i < j among the 4 estimated equations
# C(4,2) = 6 restrictions
eq_names5 <- c("housing", "food", "foodsvc", "health")
lnp_names5 <- c("lnp_h", "lnp_f", "lnp_s", "lnp_c")

sym_restrictions <- character(0)
for (i in 1:(length(eq_names5) - 1)) {
  for (j in (i + 1):length(eq_names5)) {
    sym_restrictions <- c(sym_restrictions,
      paste0(eq_names5[i], "_", lnp_names5[j], " - ",
             eq_names5[j], "_", lnp_names5[i], " = 0"))
  }
}

aids5_data <- wide_disagg %>%
  select(w_housing, w_food, w_foodsvc, w_health,
         lnp_h, lnp_f, lnp_s, lnp_c, lnXP5, trend) %>%
  drop_na()

sur5 <- systemfit(system5, method = "SUR", data = aids5_data,
                  restrict.matrix = sym_restrictions)

cat("
===== 5-Good AIDS SUR Results =====
")
summary(sur5)

# ---------- 17. Extract coefficients ----------
coefs5 <- summary(sur5)$coefficients
coef5_df <- tibble(
  term = rownames(coefs5),
  estimate = coefs5[, "Estimate"],
  std_error = coefs5[, "Std. Error"],
  t_value = coefs5[, "t value"],
  p_value = coefs5[, "Pr(>|t|)"]
)

# ---------- 18. Compute elasticities at sample means ----------
good_names5 <- c("Housing & Utilities", "Food at Home",
                 "Food Services & Accommodations", "Health Care",
                 "Everything Else")
w_cols5 <- c("w_housing", "w_food", "w_foodsvc", "w_health")

w_means5 <- colMeans(aids5_data[, w_cols5])
w_resid5_mean <- 1 - sum(w_means5)

n_est <- length(eq_names5)

# Extract betas and own-price gammas
betas5 <- sapply(eq_names5, function(eq) coef(sur5)[paste0(eq, "_lnXP5")])
gammas_own5 <- sapply(seq_along(eq_names5), function(i) {
  coef(sur5)[paste0(eq_names5[i], "_", lnp_names5[i])]
})

# Expenditure elasticities for estimated goods
e_exp5 <- 1 + betas5 / w_means5

# Residual expenditure elasticity from adding-up
beta_resid5 <- -sum(betas5)
e_exp_resid5 <- 1 + beta_resid5 / w_resid5_mean

# Marshallian own-price elasticities
e_M5 <- -1 + gammas_own5 / w_means5 - betas5

# Residual own-price via adding-up
gamma_mat5 <- matrix(0, n_est, n_est)
for (i in seq_len(n_est)) {
  for (j in seq_len(n_est)) {
    gamma_mat5[i, j] <- coef(sur5)[paste0(eq_names5[i], "_", lnp_names5[j])]
  }
}
gamma_i5 <- -rowSums(gamma_mat5)
gamma_55 <- -sum(gamma_i5)
e_M_resid5 <- -1 + gamma_55 / w_resid5_mean - beta_resid5

# Hicksian own-price
e_H5 <- e_M5 + w_means5 * e_exp5
e_H_resid5 <- e_M_resid5 + w_resid5_mean * e_exp_resid5

elasticities5 <- tibble(
  good = good_names5,
  mean_share = c(w_means5, w_resid5_mean),
  expenditure_elasticity = c(e_exp5, e_exp_resid5),
  marshallian_own_price = c(e_M5, e_M_resid5),
  hicksian_own_price = c(e_H5, e_H_resid5),
  classification = classify(c(e_exp5, e_exp_resid5))
)

cat("
===== Disaggregated Elasticities at Sample Means =====
")
print(elasticities5)

# ---------- 19. Save disaggregated results ----------
write_csv(coef5_df, "data/aids5_coefficients.csv")
write_csv(elasticities5, "data/aids5_elasticities.csv")

cat("
Saved data/aids5_coefficients.csv and data/aids5_elasticities.csv
")
