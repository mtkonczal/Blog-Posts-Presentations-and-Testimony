library(tidyverse)


data <- pc %>%
  mutate(log_GDP = log(real_gdp)) %>%
  filter(date >= "2022-04-01")


# 1. Compute difference between last and first point
diff_log <- data %>%
  summarize(diff_val = log_GDP[date == max(date)] - log_GDP[date == min(date)]) %>%
  pull(diff_val)

# 2. Divide by the number of intervals (rows - 1)
slope <- diff_log / (nrow(data) - 1)

# 3. Generate a trend that starts at log_GDP[1] and increments by slope
data <- data %>%
  mutate(log_GDP_trend = log_GDP[1] + (row_number() - 1) * slope)


data %>%
  ggplot(aes(date, log_GDP_trend)) + geom_line()

data %>%
  mutate(date = date %m+% months(2)) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = log_GDP, ymax = log_GDP_trend),
              fill = "lightgreen", alpha = 0.4) +
  geom_line(aes(y = log_GDP), color = "darkgreen", size = 1) +
  geom_line(aes(y = log_GDP_trend), color = "black", size = 1) +
  # Label the lines by placing text at the maximum date
  geom_text(
    data = data %>% filter(date == "2023-10-01"),
    aes(x = date, y = log_GDP, label = "Actual GDP"),
    color = "darkgreen", vjust = -1, size = 8
  ) +
  geom_text(
    data = data %>% filter(date == "2023-01-01"),
    aes(x = date, y = log_GDP_trend, label = "GDP trend"),
    color = "black", vjust = 2.7, size = 8
  ) +
  labs(
    title = "The 2020s Sacrifice Ratio was -0.06; Most Estimates are between 2 and 6",
    subtitle = "YoY PCE inflation fell 4.5% as GDP was 0.27% above trend during this disinflation",
    x = NULL,
    y = "log GDP",
    caption = "Modified Ball (1994) Methodology; Mike Konczal"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  )

ggsave("g1.png", width = 12, height=6.75, dpi="retina")