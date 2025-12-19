library(tidyverse)
library(tidyusmacro)
library(lubridate)
library(scales)


df_hc_base <- getFRED(hc = "USEHS", ces = "PAYEMS", gov = "USGOVT") %>%
  mutate(
    hc = hc - lag(hc, 1),
    ces = ces - lag(ces, 1),
    gov = gov - lag(gov, 1),
    rest = ces - hc - gov
  )

df <- df_hc_base %>%
  select(-ces) %>%
  pivot_longer(hc:rest, names_to = "type", values_to = "value") %>%
  filter(year(date) >= 2024)

df %>%
  ggplot(aes(x = date, y = value, fill = type)) +
  geom_col(position = "stack") +
  geom_text(
    data = df %>% filter(year(date) == 2025),
    aes(label = comma(round(value))),
    position = position_stack(vjust = 0.5),
    color = "#1a1a1a", # dark, readable on all fills
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "It's All Held Together By Health Care Jobs in 2025",
    subtitle = "CES Data, Thousands.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "CES. Seasonally adjusted. Total nonfarm. Mike Konczal."
  ) +
  scale_fill_manual(
    values = c(
      hc = "#4C72B0", # muted blue
      gov = "#55A868", # muted green
      rest = "#C44E52" # muted red
    ),
    labels = c(
      hc = "Private Edu/Health Care",
      gov = "Government",
      rest = "All Other Private"
    ),
    limits = c("hc", "gov", "rest")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  ) +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = tidyusmacro::date_breaks_gg(6, max(df$date))
  )

df_hc_base %>%
  mutate(year = year(date)) %>%
  filter(year >= 2019) %>%
  group_by(year) %>%
  reframe(percent_hc = sum(hc) / sum(hc, gov, rest))

df_hc_base %>%
  mutate(year = year(date)) %>%
  filter(year >= 2019) %>%
  group_by(year) %>%
  reframe(percent_hc = (sum(hc) + sum(gov)) / sum(hc, gov, rest))


# 1. Fetch and adjust data
df_base <- getFRED(mining = "USMINE", cons = "USCONS", manfact = "MANEMP")

df_cleaned <- df_base %>%
  mutate(
    m_idx = interval(as.Date("2024-03-01"), date) %/% months(1),
    adj_weight = case_when(
      date <= "2024-03-01" ~ 0,
      date <= "2025-03-01" ~ m_idx / 12,
      date > "2025-03-01" ~ 1
    ),
    mining = mining + (adj_weight * -4),
    cons = cons + (adj_weight * -29),
    manfact = manfact + (adj_weight * -95)
  ) %>%
  select(-m_idx, -adj_weight)

# 2. Preparation for labels
facet_names <- c(
  "mining" = "Mining and Logging",
  "cons" = "Construction",
  "manfact" = "Manufacturing"
)

# 3. Plotting
df_cleaned %>%
  pivot_longer(mining:manfact, names_to = "type", values_to = "value") %>%
  mutate(
    project = logLinearProjection(
      date = date,
      value = value,
      start_date = "2022-12-01",
      end_date = "2024-12-01",
      group = type
    )
  ) %>%
  filter(year(date) >= 2022) %>%
  ggplot() +
  # Solid line for revised actuals
  geom_line(aes(date, value, color = type), linewidth = 1.1) +
  # Dashed line for trend projection
  geom_line(
    aes(date, project, color = type),
    linetype = "dashed",
    linewidth = 0.8,
    alpha = 0.8
  ) +
  facet_wrap(~type, scales = "free", labeller = as_labeller(facet_names)) +
  scale_color_brewer(palette = "Set1") + # Cleaner, high-contrast colors
  scale_y_continuous(labels = comma) +
  labs(
    title = "So Cool We'll All Be Poorer to Do This",
    subtitle = "Revised levels with preliminary 2025 benchmarks. Dashed lines represent Dec 2022 to Dec 2024 log-linear trends.",
    caption = "Source: BLS Preliminary Benchmark Revisions (March 2025). Linear wedge applied back to April 2024.",
    x = NULL,
    y = "Employees (Thousands)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title.position = "plot", # Aligns title to the left edge of the plot
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 11), # Nicer facet titles
    panel.grid.minor = element_blank()
  )
