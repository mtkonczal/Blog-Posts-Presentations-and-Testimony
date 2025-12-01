library(tidycensus)
library(tidyverse)

# API key is loaded in environment.

# --- Variables ---
vars <- c(
  rent = "B25064_001E",
  total_pop = "B01003_001E",
  foreign_born = "B05002_013E",
  home_value = "B25077_001E"
)

# --- Fetch US Data ---
years <- c(2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024)

us_data <- map_dfr(years, function(yr) {
  get_acs(
    geography = "us",
    year = yr,
    variables = vars,
    survey = "acs1",
    output = "wide"
  ) %>%
    mutate(year = yr)
})

# --- Growth Rates ---
trend_data <- us_data %>%
  arrange(year) %>%
  mutate(
    rent_growth = (rent - lag(rent)) / lag(rent),
    home_value_growth = (home_value - lag(home_value)) / lag(home_value),
    prev_fb = lag(foreign_born),
    prev_pop = lag(total_pop),
    immigrant_inflow = (foreign_born - prev_fb) / prev_pop
  ) %>%
  filter(year %in% c(2017, 2018, 2019, 2022, 2023, 2024))

# --- Plot Data ---
plot_data <- trend_data %>%
  select(year, rent_growth, home_value_growth, immigrant_inflow) %>%
  pivot_longer(
    cols = c(rent_growth, home_value_growth, immigrant_inflow),
    names_to = "metric_code",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric_code,
      rent_growth = "Median Rent: Year-over-Year Growth",
      home_value_growth = "Median Home Values: Year-over-Year Growth",
      immigrant_inflow = "Immigrant Inflow: Share of Population"
    ),
    metric = factor(
      metric,
      levels = c(
        "Median Rent: Year-over-Year Growth",
        "Median Home Values: Year-over-Year Growth",
        "Immigrant Inflow: Share of Population"
      )
    )
  )

# --- Plot ---
ggplot(plot_data, aes(x = year, y = value)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    color = "red",
    alpha = 0.6
  ) +
  geom_line(size = 1.3, color = "grey30") +
  geom_point(size = 2.5, color = "black") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2022, 2023, 2024)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Figure 1: Rent and Home-Price Growth Spiked Before Immigration Inflow",
    subtitle = "Comparing Rent and Median Home-Value Growth vs. Immigration Inflow, Entire USA",
    x = NULL,
    y = NULL,
    caption = "Source: ACS 1-year estimates. 2020 not recorded in ACS; 2021 is thereby missing. Mike Konczal"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )
