# Code to download NIPA tables from BEA.
# This downloads detailed PCE inflation data from 2020-2023.
# Make sure to have run: 0_a_helper_functions.R first.

# Written by: Mike Konczal
# Last modified: 9/1/2023
library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

#### Downloads ####
# This calls a helper function from the other file for years 2020-2023
PCE_Weight <- get_NIPA_data(beaKey, 'U20405', 'M', '2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
PCE_Weight <- BEA_date_monthly(PCE_Weight)

GDP_Weight <- PCE_Weight %>% filter(SeriesCode == "DPCERC") %>%
  select(date, TotalGDP = DataValue)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="date") %>%
  # The weight is approximated as nominal consumption shares as a percent of the total.
  mutate(PCEweight = DataValue/TotalGDP) %>%
  select(date, LineDescription, PCEweight)

pce <- get_NIPA_data(beaKey, 'U20404', 'M', '2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
pce <- BEA_date_monthly(pce)

PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'M', '2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
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

#save(pce, file = "data/pce.RData")

