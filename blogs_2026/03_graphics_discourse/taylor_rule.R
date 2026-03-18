library(tidyverse)
library(tidyusmacro)
library(scales)
library(lubridate)

# Prepare data
df <- getFRED(unrate = "unrate",
              core_pce = "PCEPILFE",
              fed_funds = "FEDFUNDS",
              keep_all = FALSE) %>%
  mutate(core_pce = core_pce / lag(core_pce, 12) - 1,
         unrate = unrate / 100,
         neutral_rate = 0.01,
         unrate_star = 0.042,
         pce_target = 0.02,
         fed_funds = fed_funds / 100) %>%
  mutate(taylor = neutral_rate + core_pce +
           0.5 * (core_pce - pce_target) + (unrate_star - unrate)) %>%
  filter(year(date) >= 2023)

# Latest values for labeling
label_point <- df %>% filter(date == max(date))

# Date breaks for x-axis: every 6 months, ending at max
max_date <- max(df$date)
min_date <- max_date %m-% months(30)
date_breaks <- seq(min_date, max_date, by = "6 months")

# Extend x-axis space for labels
future_date <- max_date + months(2)

# Plot
ggplot(df) +
  geom_line(aes(date, taylor), color = "red", size = 1) +
  geom_line(aes(date, fed_funds), color = "navy", size = 1) +
  geom_text(data = label_point, aes(date, taylor, label = "Taylor\nRule"),
            color = "red", vjust = 1, nudge_x = 45, size = 5) +
  geom_text(data = label_point, aes(date, fed_funds, label = "Fed Funds\nRate"),
            color = "navy", nudge_x = 50, size = 5) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(
    breaks = date_breaks,
    labels = ~ format(., "%b\n%Y")
  ) +
  coord_cartesian(clip = "off") +
  expand_limits(x = future_date) +
  labs(
    title = "Effective Fed Funds Rate vs Taylor Rule Estimate",
    subtitle = "The Taylor Rule suggests rates were quite elevated in 2024",
    y = "Rate (Percent)",
    x = NULL,
    caption = "Source: FRED, author's calculations, SEP for neutral rate and natural unemployment."
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.margin = margin(10, 50, 10, 10),
    plot.title.position = "plot")
