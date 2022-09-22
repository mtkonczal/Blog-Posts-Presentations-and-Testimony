library(bea.R)
library(tidyverse)
library(lubridate)

get_NIPA_data <- function(beaKey, TableName, Frequency, Year, data_set_name = 'NIPA'){
  NIPA_request <- list(
    'UserID' = beaKey ,
    'Method' = 'GetData',
    'datasetname' = data_set_name,
    'TableName' = TableName,
    'Frequency' = Frequency,
    'Year' = Year,
    'ResultFormat' = 'json'
  );
  NIPA_data <- beaGet(NIPA_request, asWide = FALSE)
  return(NIPA_data)
}

BEA_date_quarterly <- function(x){
  x <- x %>%
  mutate(year = substr(TimePeriod, 1, 4)) %>%
  mutate(quarter = substr(TimePeriod, 5,6)) %>%
  mutate(month = case_when(
    quarter == "Q1" ~ 3,
    quarter == "Q2" ~ 6,
    quarter == "Q3" ~ 9,
    quarter == "Q4" ~ 12))
  x$date <- paste(x$month, "01", x$year, sep="/")
  x$date <- as.Date(x$date, "%m/%d/%Y")
  x <- x %>% select(-month, -quarter, -year)
  return(x)
}

BEA_date_monthly <- function(x){
  x <- x %>%
    mutate(year = substr(TimePeriod, 1, 4)) %>%
    mutate(month = substr(TimePeriod, 6,7))
  x$date <- paste(x$month, "01", x$year, sep="/")
  x$date <- as.Date(x$date, "%m/%d/%Y")
  x <- x %>% select(-month, -year)
  return(x)
}



draw_Trendline <- function(x, trend_start, trend_end, freq){
  
  trend_start <- mdy(trend_start)
  trend_end <- mdy(trend_end)
  trend_period <- interval(trend_start, trend_end)
  trend_period = trend_period %/% months(1)
  
  x <- x %>%
    group_by(SeriesCode) %>%
    # Calculate trend
    mutate(trend_Vbegin = DataValue[date==trend_start], trend_Vend = DataValue[date==trend_end]) %>%
    mutate(trend_calc = (trend_Vend/trend_Vbegin)^(1/(trend_period/12))) %>%
    # Begin projections
    mutate(after_check = (date > trend_end)) %>%
    mutate(trend_counter = freq*cumsum(after_check)) %>%
    mutate(trend_multipler = trend_calc^(trend_counter/12)) %>%
    mutate(trendline = ifelse(after_check == 0, NA, trend_multipler*DataValue[date==trend_end]) ) %>%
    mutate(trendline = ifelse(date == trend_end, DataValue, trendline)) %>%
    ungroup() %>%
    select(-trend_Vbegin, -trend_Vend, -trend_calc, -after_check, -trend_multipler, -trend_counter)
  
  return(x)
}
