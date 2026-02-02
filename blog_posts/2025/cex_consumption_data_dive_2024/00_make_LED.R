library(tidyverse)
library(janitor)
library(tidyusmacro)
library(tibble)


led <- read_csv("data/cex_data.csv")

food_items <- c("Food at home", "Food away from home")

led_base <- led %>%
  filter(
    item_text %in% food_items,
    characteristics_text != "All Consumer Units"
  ) %>%
  select(
    year,
    value,
    item_text,
    demographics_text,
    characteristics_text,
    total_expend
  )

all_other <- led_base %>%
  group_by(year, demographics_text, characteristics_text) %>%
  summarise(
    value = first(total_expend) - sum(value, na.rm = TRUE),
    total_expend = first(total_expend),
    .groups = "drop"
  ) %>%
  mutate(item_text = "All other expenditures")

led_final <- bind_rows(
  led_base,
  all_other
)

food_prices <- getFRED(
  all_less_food = "CPIULFSL",
  food_away_from_home = "CUSR0000SEFV",
  food_at_home = "CUSR0000SAF11"
)

food_prices_annual <- food_prices %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    across(
      c(all_less_food, food_away_from_home, food_at_home),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "price_series",
    values_to = "cpi_index"
  )

price_map <- tibble(
  price_series = c("food_at_home", "food_away_from_home", "all_less_food"),
  item_text = c("Food at home", "Food away from home", "All other expenditures")
)

food_prices_annual <- food_prices_annual %>%
  left_join(price_map, by = "price_series") %>%
  select(year, item_text, cpi_index)

merged <- led_final %>%
  left_join(food_prices_annual, by = c("year", "item_text"))

write_csv(merged, "data/food_led_data.csv")
