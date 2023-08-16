###############################################################
# Graphics that I'm watching, Early 2023, inflation
# Mike Konczal
# Last updated 2/13/2023

library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)
library(tidytext)
library(viridis)
library(bea.R)
library(ggridges)

#Either run Script 1 to download data fresh, or used locally stored data.
source(file = "/Users/mkonczal/Documents/GitHub/Blog-Posts-Presentations-and-Testimony/House_Hearing_Mar_9_2023/0_helper_functions.R")
beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)

#### GRAPHIC 2

g_s <- c("DGDSRX", "DSERRX")
BEA_T20806 <- get_NIPA_data(beaKey, 'T20806', 'M', 'All')
BEA_T20806 <- BEA_date_monthly(BEA_T20806)


##### NORMALIZED ####
normalized <- BEA_T20806 %>% filter(SeriesCode %in% g_s) %>%
  filter(date >= "2013-12-01") %>%
  group_by(SeriesCode) %>%
  mutate(DataValue = DataValue/DataValue[date=="2020-01-01"]) %>%
  draw_ll_Trendline("12/1/2014", "12/1/2019",12) %>%
  ungroup() %>% mutate(category = "Normalized (01/2020 = 100)")


graphic_shift <- normalized %>%  
  ggplot(aes(date,DataValue,color=LineDescription)) + theme_classic() + geom_line(size=1.2) +
  geom_line(aes(date,trendline), size=0.6, linetype="longdash") +
  labs(title=NULL,
       x=NULL,
       y="Jan 2020 = 1",
       subtitle=NULL,
       caption=NULL) +
#       subtitle="Real Personal Consumption Expenditures by Major Type of Product, Monthly, Chained Dollars, Jan 2020 = 1",
#       caption="Table 2.8.6, log-linear trendline from 2015-2019, author's calculation, Mike Konczal, Roosevelt Institute")
  scale_fill_brewer(palette="Paired") + theme(legend.position = c(0.3,0.8), legend.title = element_blank())



PCE_Items <- get_NIPA_data(beaKey, 'U20404', 'Q', '2016,2017,2018,2019,2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'Q', '2016,2017,2018,2019,2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail') %>%
  select(LineDescription, TimePeriod, Quantity = DataValue)

PCE_Items <- PCE_Items %>%
  left_join(PCE_Q, by=c('TimePeriod' = 'TimePeriod','LineDescription' = 'LineDescription')) %>%
  rename(Price = DataValue)
PCE_Items <- BEA_date_quarterly(PCE_Items) %>% mutate(year = year(date), year = as.factor(year))

g_s_index <- c("Goods","Services")

convex_curve <- PCE_Items %>% filter(LineDescription %in% g_s_index) %>% arrange(date) %>%
  mutate(TimePeriod = str_replace_all(TimePeriod,"Q","\nQ")) %>%
  mutate(TimePeriod_a = ifelse(month(date)==12 & year(date)>2018,TimePeriod,NA)) %>%
  ggplot(aes(Quantity, Price, label=TimePeriod_a)) + geom_point() + geom_path() + theme_classic() +
  geom_label(size=2) + facet_grid(~LineDescription, scales = "free") +
  labs(title=NULL, subtitle=NULL,
       x="Quantity (Index, 2012=100)", y="Price (Index, 2012=100)",
       caption=NULL)

# Nominal growth for goods and services categories, split into their price and quantity indexes, national accounts, quarterly


###### PCE CHANGES
########### THE BIG ONE
PCE_Weight <- get_NIPA_data(beaKey, 'U20405', 'M', '2017,2018,2019,2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
PCE_Weight <- BEA_date_monthly(PCE_Weight)

GDP_Weight <- PCE_Weight %>% filter(SeriesCode == "DPCERC") %>%
  select(date, TotalGDP = DataValue)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="date") %>%
  # The weight is approximated as nominal consumption shares as a percent of the total.
  mutate(PCEweight = DataValue/TotalGDP) %>%
  select(date, LineDescription, PCEweight)

pce <- get_NIPA_data(beaKey, 'U20404', 'M', '2017,2018,2019,2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
pce <- BEA_date_monthly(pce)

PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'M', '2017,2018,2019,2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- BEA_date_monthly(PCE_Q) %>% select(LineDescription, date, Quantity = DataValue)

pce <- pce %>%
  left_join(PCE_Weight, by=c('date' = 'date','LineDescription' = 'LineDescription')) %>%
  left_join(PCE_Q, by=c('date' = 'date','LineDescription' = 'LineDescription'))

pce <- pce %>%
  group_by(SeriesCode) %>%
  mutate(DataValue_P1 = (DataValue - lag(DataValue,1))/lag(DataValue,1)) %>%
  # Use the lagged weight
  mutate(WDataValue_P1 = DataValue_P1*lag(PCEweight,1)) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^12-1) %>%
  mutate(Quantity_P1 = (Quantity - lag(Quantity,1))/lag(Quantity,1)) %>%
  ungroup()

rm(PCE_Weight, GDP_Weight, PCE_Q)

##### MAKE TABLE ####

# TABLE 1
headline_categories <- c("PCE excluding food and energy","Services")

decline_pce <- pce %>% filter(LineDescription %in% c(headline_categories)) %>%
  rename(item_name = LineDescription, value = DataValue) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value, 1)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Pchange3a = (1 + Pchange3)^4 - 1) %>%
  mutate(Pchange6a = (1 + Pchange6)^2 - 1) %>%
  mutate(YoY = (value/lag(value,12) - 1)) %>%
  mutate(TwoYear = value/lag(value,24)-1) %>%
  mutate(TwoYearA = (1+TwoYear)^(1/2)-1) %>%
  ungroup() %>%
  select(date, item_name, Pchange3a, Pchange6a, YoY) %>%
  group_by(item_name) %>%
  mutate(June2022_values = Pchange6a[date=="2022-06-01"]) %>%
  mutate(Values_2021 = YoY[date=="2021-12-01"]) %>%
  ungroup() %>%
  filter(date == max(date))

inflation_declining <- decline_pce[-1,] %>%
  mutate(Pchange3a = percent(Pchange3a), Pchange6a = percent(Pchange6a), June2022_values = percent(June2022_values)) %>%
  mutate(item_name = str_replace_all(item_name, "All items less food, shelter, energy, and used cars and trucks","Core inflation less shelter and used cars")) %>%
  arrange(item_name)

decline_pce <- decline_pce %>%
  select(`Category Name, CPI` = item_name, `1st Half, 2022` = June2022_values, `2nd Half, 2022` = Pchange6a, `Last 3 Months` = Pchange3a)
