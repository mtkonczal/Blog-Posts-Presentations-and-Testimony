library(ggplot2)
library(tidyr) # For reshaping data for plotting

# ---------------------------------------------------------
# 1. SETUP AND DATA LOADING
# ---------------------------------------------------------
# Load data
df <- read.csv('for_computation_babies.csv')

# Use the 'earlier data' (truncate to first 11 rows)
df_short <- df[1:11, ]

# Parameters
beta_val <- 0.96^2
phi_val <- 0.0

# ---------------------------------------------------------
# 2. ESTIMATION FUNCTION (Backward Induction)
# ---------------------------------------------------------
estimate_penalty <- function(df, cohort_col, beta = 0.9216, phi = 0.0) {
  B <- df[[cohort_col]]
  ages <- df$age

  # Calculate flows b_t (B_{t+1} - B_t)
  b <- diff(B)
  n <- length(b)

  # Initialize Z vector
  Z <- numeric(n)

  # Terminal condition
  Z_next <- 0.0

  # Backward Loop
  # Iterate from n down to 1
  for (i in n:1) {
    B_next <- B[i + 1] # Stock at t+1

    # Marginal Utility U'(B) = 1/B
    current_joy <- (beta / 2.0) * ((1.0 / B_next) - phi)

    # Euler Equation
    Z[i] <- current_joy + beta * Z_next

    # Update Z_next
    Z_next <- Z[i]
  }

  # Recover Penalty
  p <- Z / b

  # Return list of results (ages matches length of p)
  list(ages = head(ages, -1), p = p)
}

# ---------------------------------------------------------
# 3. RUN ESTIMATION FOR BOTH COHORTS
# ---------------------------------------------------------
# Estimate for 1980
results_1980 <- estimate_penalty(
  df_short,
  'yb1980',
  beta = beta_val,
  phi = phi_val
)

# Estimate for 1986
results_1986 <- estimate_penalty(
  df_short,
  'yb1986',
  beta = beta_val,
  phi = phi_val
)

# ---------------------------------------------------------
# 4. PREPARE DATA FOR PLOTTING
# ---------------------------------------------------------
# Create a data frame combining both results
plot_df <- data.frame(
  Age = results_1980$ages,
  Penalty_1980 = results_1980$p,
  Penalty_1986 = results_1986$p
)

# Reshape to "long" format for ggplot (requires 'tidyr')
# If you don't have tidyr, you can build the data frame manually
plot_df_long <- pivot_longer(
  plot_df,
  cols = c("Penalty_1980", "Penalty_1986"),
  names_to = "Cohort",
  values_to = "Penalty"
)

# Clean up names for the legend
plot_df_long$Cohort <- ifelse(
  plot_df_long$Cohort == "Penalty_1980",
  "1980 Cohort",
  "1986 Cohort"
)

# Print the comparison table
print(plot_df)

# ---------------------------------------------------------
# 5. PLOTTING
# ---------------------------------------------------------
ggplot(
  plot_df_long,
  aes(x = Age, y = Penalty, color = Cohort, shape = Cohort, linetype = Cohort)
) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("1980 Cohort" = "#1f77b4", "1986 Cohort" = "#ff7f0e")
  ) +
  theme_minimal(base_size = 18) +
  labs(
    title = "Estimated Adjustment Penalties (p_t) by Cohort",
    subtitle = "Comparing friction costs for 1980 vs 1986 birth cohorts",
    y = "Penalty Parameter (p)",
    x = "Age",
    caption = "Rough estimate, Mike Konczal"
  ) +
  theme(legend.position = "top",
        plot.title.position = "plot")
