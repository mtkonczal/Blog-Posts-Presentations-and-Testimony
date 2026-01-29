library(tidyverse)
library(lubridate)
library(tidyusmacro)
library(scales)

#ces <- getBLSFiles("ces", "rortybomb@gmail.com")
#ces$data_type_code <- as.numeric(ces$data_type_code)

write_rds(ces, "ces.rds")


# goods
goods <- ces %>%
  filter(
    data_type_code == 1,
    seasonal == "S",
    industry_name == "Goods-producing"
  )

goods %>%
  filter(year(date) >= 2025) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2025-04-01"), linetype = "dotted") +
  theme_esp() +
  labs(subtitle = "Total Goods-Producing Jobs")

goods %>%
  mutate(
    projection = logLinearProjection(date, value, "2022-12-01", "2024-12-01")
  ) %>%
  filter(year(date) >= 2023) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  geom_line(aes(date, projection), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2025-04-01"), linetype = "dotted") +
  theme_esp() +
  labs(
    subtitle = "Total Goods-Producing Jobs",
    title = "Looks Worse With Projection"
  )


# goods
ces %>%
  filter(
    data_type_code == 1,
    seasonal == "S",
    industry_name %in%
      c(
        "Construction",
        "Mining and logging",
        "Durable goods",
        "Nondurable goods"
      )
  ) %>%
  group_by(industry_name) %>%
  mutate(change = value - lag(value, 1)) %>%
  ungroup() %>%
  #  filter(date >= 2025-04-01 | date <= 2024-12-01) %>%
  filter(year(date) >= 2024) %>%
  group_by(industry_name, year) %>%
  reframe(change = mean(change))

# goods
ces %>%
  filter(
    data_type_code == 1,
    seasonal == "S",
    industry_name %in% c("Durable goods", "Nondurable goods")
  ) %>%
  group_by(date) %>%
  reframe(
    value = sum(value)
  ) %>%
  ungroup() %>%
  mutate(
    change = value - lag(value, 1),
    projection = logLinearProjection(date, value, "2022-12-01", "2024-12-01")
  ) %>%
  #  filter(date >= 2025-04-01 | date <= 2024-12-01) %>%
  filter(year(date) >= 2023) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  geom_line(aes(date, projection), linetype = "dotted") +
  labs(subtitle = "Durable and Nondurable Goods Continue Their Slowdown") +
  theme_esp()
