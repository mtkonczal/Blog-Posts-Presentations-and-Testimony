library(bea.R)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)
library(tidytext)

beaKey <- ENTER HERE
bea_1_15_request <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T11500',
  'Frequency' = 'Q',
  'Year' = '2018,2019,2020,2021,2022',
  'ResultFormat' = 'json'
);
bea_1_15_data <- beaGet(bea_1_15_request, asWide = FALSE)

bea_1_15_data <- bea_1_15_data %>%
  mutate(year = substr(TimePeriod, 1, 4)) %>%
  mutate(quarter = substr(TimePeriod, 5,6)) %>%
  mutate(month = case_when(
    quarter == "Q1" ~ 3,
    quarter == "Q2" ~ 6,
    quarter == "Q3" ~ 9,
    quarter == "Q4" ~ 12))
bea_1_15_data$date <- paste(bea_1_15_data$month, "01", bea_1_15_data$year, sep="/")
bea_1_15_data$date <- as.Date(bea_1_15_data$date, "%m/%d/%Y")


filter_1_15 <- c('Compensation of employees (unit labor cost)', 'Unit nonlabor cost', 'Corporate profits with IVA and CCAdj (unit profits from current production)')

# By Lagged
bea_1_15 <- bea_1_15_data %>%
  filter(date > "2019-12-01") %>%
  filter(LineDescription %in% filter_1_15) %>%
  mutate(item_name = case_when(
    LineDescription == 'Compensation of employees (unit labor cost)' ~ 'Labor Cost',
    LineDescription == 'Unit nonlabor cost' ~ 'Nonlabor Cost',
    LineDescription == 'Corporate profits with IVA and CCAdj (unit profits from current production)' ~ 'Corporate Profits',
  )) %>%
  group_by(item_name) %>%
  mutate(DataValueLagged = DataValue - lag(DataValue,1)) %>%
  ungroup() %>%
  mutate(item_name = factor(item_name, levels = c("Corporate Profits", "Nonlabor Cost", "Labor Cost"))) %>%
  ggplot(aes(x = date, y = DataValueLagged, fill = item_name)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "",
       subtitle = "",
       caption ="") +
  scale_fill_brewer(palette="Set1") +
  scale_x_date(date_labels = "%b %Y")
