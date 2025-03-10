library(tidyverse)


sr <- read_csv("sacrifice_ratio_data.csv")




sr80 <- sr %>% 
  filter(period == 1980)

ggplot(sr80, aes(x = date)) +
  geom_ribbon(aes(ymin = log_GDP, ymax = log_GDP_trend),
              fill = "pink", alpha = 0.4) +
  geom_line(aes(y = log_GDP), color = "red", size = 1) +
  geom_line(aes(y = log_GDP_trend), color = "black", size = 1) +
  # Label the lines by placing text at the maximum date
  geom_text(
    data = sr80 %>% filter(date == "1982-01-01"),
    aes(x = date, y = log_GDP, label = "Actual GDP"),
    color = "red", vjust = 1.75, size = 8
  ) +
  geom_text(
    data = sr80 %>% filter(date == "1982-01-01"),
    aes(x = date, y = log_GDP_trend, label = "GDP trend"),
    color = "black", vjust = -3, size = 8
  ) +
  labs(
    title = "Figure 1: The 1980s Sacrifice Ratio was 1.95",
    subtitle = "CPI inflation fell 8.5% as GDP was 16.6% below trend during this disinflation",
    x = NULL,
    y = "log GDP",
    caption = "Mike Konczal"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  )


ggsave("figure1_fixed.png", units = c("px"), width=2145, height = 1067)


sr20 <- sr %>% 
  filter(period == 2020)

ggplot(sr20, aes(x = date)) +
  geom_ribbon(aes(ymin = log_GDP, ymax = log_GDP_trend),
              fill = "lightgreen", alpha = 0.4) +
  geom_line(aes(y = log_GDP), color = "darkgreen", size = 1) +
  geom_line(aes(y = log_GDP_trend), color = "black", size = 1) +
  # Label the lines by placing text at the maximum date
  geom_text(
    data = sr20 %>% filter(date == "2023-10-01"),
    aes(x = date, y = log_GDP, label = "Actual GDP"),
    color = "darkgreen", vjust = -1, size = 8
  ) +
  geom_text(
    data = sr20 %>% filter(date == "2023-01-01"),
    aes(x = date, y = log_GDP_trend, label = "GDP trend"),
    color = "black", vjust = 2.4, size = 8
  ) +
  labs(
    title = "Figure 2: The 2020s Sacrifice Ratio was -0.04",
    subtitle = "Trend CPI inflation fell 3.2% as GDP was 0.1% above trend during this disinflation",
    x = NULL,
    y = "log GDP",
    caption = "Mike Konczal"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  )