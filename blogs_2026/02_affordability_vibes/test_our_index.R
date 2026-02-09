library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)

test_index <- pce %>%
  group_by(item_name) %>%
  mutate(DataValue_P1 = (value - lag(value, 1)) / lag(value, 1)) %>%
  # Use the lagged weight
  mutate(WDataValue_P1 = DataValue_P1 * lag(PCEweight, 1)) %>%
  mutate(WDataValue_P1a = (1 + WDataValue_P1)^12 - 1) %>%
  ungroup()


#First make a graphic of non-housing services
#### Basic Index ####
nhs_index <-
  test_index %>%
  select(date, LineDescription = item_name, WDataValue_P1, PCEweight) %>%
  group_by(date) %>%
  summarize(
    nhsWP1 = WDataValue_P1[LineDescription == "PCE services excluding energy"] -
      WDataValue_P1[LineDescription == "Housing"],
    nhs_weight = PCEweight[LineDescription == "PCE services excluding energy"] -
      PCEweight[LineDescription == "Housing"],
  ) %>%
  ungroup() %>%
  mutate(nhsWP1A = nhsWP1 / nhs_weight) %>%
  mutate(nhsWP1A = (nhsWP1A + 1)^12 - 1) %>%
  mutate(index = nhsWP1 / nhs_weight + 1) %>%
  filter(!is.na(index)) %>%
  mutate(index = cumprod(index))
#####

#First make a graphic of non-housing services
#### Basic Index ####
essentials_index <-
  test_index %>%
  select(date, LineDescription = item_name, WDataValue_P1, PCEweight) %>%
  group_by(date) %>%
  summarize(
    nhsWP1 = WDataValue_P1[LineDescription == "Housing and utilities"] +
      WDataValue_P1[
        LineDescription ==
          "Food and beverages purchased for off-premises consumption"
      ],
    nhs_weight = PCEweight[LineDescription == "Housing and utilities"] +
      PCEweight[
        LineDescription ==
          "Food and beverages purchased for off-premises consumption"
      ],
  ) %>%
  ungroup() %>%
  mutate(nhsWP1A = nhsWP1 / nhs_weight) %>%
  mutate(index = nhsWP1 / nhs_weight + 1) %>%
  filter(!is.na(index)) %>%
  mutate(
    index = cumprod(index),
    index_2019 = index / index[date == "2019-12-01"]
  )
#####

tail(essentials_index$index_2019)

tk <- all_indices %>% filter(index_type == "Core Essentials")

tail(tk$index_norm / 100, 10) - tail(essentials_index$index_2019, 10)
