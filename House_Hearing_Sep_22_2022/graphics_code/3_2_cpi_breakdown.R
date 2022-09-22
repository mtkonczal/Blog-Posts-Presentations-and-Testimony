###############################################################
# Code to evaluate the 'Handoff Theory' of inflation normalizing in 2021-2022.
# Mike Konczal
# Last updated 4/10/22

setwd("/Users/mkonczal/Documents/GitHub/Presentations-and-Testimony/House_Hearing_Sep_22_2022/")
library(tidyverse)
library(lubridate)
library(scales)

##### SET UP SOME THINGS #####
source(file = "graphics_code/3_1_load_cpi_data.R")


cpi0 <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Wchange6 = (Pchange3*weight)/100) %>%
  mutate(Wchange6a = (1 + Wchange3)^2 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()

months2022 <- interval(ymd("2021-12-01"), max(cpi0$date))
months2022 = months2022 %/% months(1)

average_month_pre_pandemic <- cpi0 %>%
  filter(date == "2014-01-01" | date == "2019-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(pre_value = (1+ (value/lag(value)-1)*weight/100)^(1/6)-1) %>%
  ungroup() %>%
  filter(!is.na(pre_value))

value_2021 <- cpi0 %>%
  filter(date == "2020-12-01" | date == "2021-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(v_2021 = (1+ (value/lag(value)-1)*weight/100)^(1/1)-1) %>%
  ungroup() %>%
  filter(!is.na(v_2021))

recovery_period_months <- interval(ymd("2020-12-01"), max(cpi0$date))
recovery_period_months = recovery_period_months %/% months(1)

recovery_period <- cpi0 %>%
  filter(date == "2020-12-01" | date == max(date)) %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(recovery_inflaton = (1+ (value/lag(value)-1)*weight/100)^(12/recovery_period_months)-1) %>%
  ungroup() %>%
  filter(!is.na(recovery_inflaton))

cpi <- cpi0 %>%
  group_by(item_name) %>%
  mutate(Pchange_2022 = value/lag(value, months2022)-1) %>%
  mutate(Wchange_2022 = (Pchange_2022*weight)/100) %>%
  mutate(Wchange_2022a = (1+Wchange_2022)^(12/months2022)-1) %>%
  select(item_name, date, value, weight, Pchange1, Wchange1a, Pchange_2022, Wchange_2022a, Wchange12) %>%
  ungroup() %>%
  left_join(average_month_pre_pandemic, by="item_name") %>%
  left_join(value_2021, by="item_name") %>%
  left_join(recovery_period, by="item_name")

graphic <- cpi %>% filter(item_name %in% c("Commodities less food and energy commodities", "Services less energy services", "Shelter"),
               date == max(date)) %>% select(item_name, pre_value, v_2021, Wchange_2022a)

graphic_sub <- graphic %>% filter(item_name != "Commodities less food and energy commodities") %>% arrange(item_name)
graphic_sub <- graphic_sub[1,2:4] - graphic_sub[2,2:4]
graphic_sub <- graphic_sub %>% mutate(item_name = "Core Services Excluding Housing") %>% relocate(item_name)

graphic <- graphic %>% filter(item_name != "Services less energy services") %>% rbind(graphic_sub) %>%
  mutate(pre_value = percent(pre_value), v_2021 = percent(v_2021), Wchange_2022a = percent(Wchange_2022a)) %>%
  select(Item = item_name, `2014-2019` = pre_value, `2021` = v_2021, `2022` = Wchange_2022a)

graphic$Item <- str_replace_all(graphic$Item, "Commodities less food and energy commodities", "Core Goods")
graphic$Item <- str_replace_all(graphic$Item, "Shelter", "Housing")