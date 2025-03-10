library(tidyverse)
library(govMacroTools)
library(lubridate)
library(broom)


proj_log_linear <- function(data, start_date, end_date) {
  
  projections <- data %>%
    filter(date >= start_date, date <= end_date) %>%
    group_by(SeriesLabel) %>%
    mutate(index_projection = row_number()) %>%
    # get log-linear regression variables
    do(tidy(lm(log(real_spending) ~ index_projection, data = .))) %>%
    select(SeriesLabel, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    # keep piping!
    inner_join(df, by="SeriesLabel") %>%
    filter(date >= start_date) %>%
    group_by(SeriesLabel) %>%
    mutate(index = row_number(),
           projection = exp(index_projection*index + `(Intercept)`)) %>%
    ungroup() %>%
    select(SeriesLabel, date, projection) %>%
    full_join(df)
  
  projections <- full_join(df, projections) %>% select(projection)
  return(projections)
  
}


proj_log_linear_single <- function(data, start_date, end_date) {

    
  projections <- data %>%
    filter(date >= start_date, date <= end_date) %>%
    mutate(index_projection = row_number()) %>%
    # get log-linear regression variables
    do(tidy(lm(log(custom_index) ~ index_projection, data = .))) %>%
    select(term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) 
  
  data2 <- data
  data2$`(Intercept)` <- projections$`(Intercept)`
  data2$index_projection <- projections$index_projection
  
  projections <- data2 %>%
    filter(date >= start_date) %>%
    mutate(index = row_number(),
           projection = exp(index_projection*index + `(Intercept)`)) %>%
    select(date, projection)
  
  projections <- full_join(data, projections) %>% select(projection)
  return(projections)
  
}




nipa <- getNIPAFiles(type = "Q")

total_gdp <- nipa %>%
  filter(TableId == "U20405", SeriesCode ==
    "DPCERC") %>%
  dplyr::select(date, TotalGDP = Value)

spending <- nipa %>%
  filter(TableId == "U20406") %>%
  select(date, SeriesLabel, real_spending = Value) %>%
  distinct(date,
           SeriesLabel,
           .keep_all = TRUE
  )

pce_weight <- nipa %>%
  filter(TableId == "U20405") %>%
  left_join(total_gdp, by = "date") %>%
  mutate(PCEweight = Value / TotalGDP) %>%
  select(date, SeriesLabel, PCEweight) %>%
  distinct(date,
    SeriesLabel,
    .keep_all = TRUE
  )

pce <- nipa %>%
  filter(TableId == "U20404") %>%
  select(date, SeriesLabel, LineNo, inflation = Value) %>%
  inner_join(spending, by = c("date", "SeriesLabel")) %>%
  inner_join(pce_weight, by = c("date", "SeriesLabel"))

tail(pce)

#comparison_array <- c("Video and audio equipment","Admissions to specified spectator amusements")
#comparison_array <- c("Food services", "Food and beverages purchased for off-premises consumption")
comparison_array <- c("Hairdressing salons and personal grooming establishments","Personal care products (part of 118)")


year_start <- 2015

total <- pce %>% filter(SeriesLabel %in% comparison_array) %>%
  group_by(date) %>%
  reframe(real_spending = sum(real_spending),
          SeriesLabel = "Total spending")

df <- pce %>% filter(SeriesLabel %in% comparison_array) %>%
  select(date, real_spending, SeriesLabel) %>%
  rbind(total)

df
df$projection <- proj_log_linear(df, "2017-01-01", "2019-09-01") %>% pull()

# Graphic 1
df %>%
  filter(year(date) >= year_start) %>%
  ggplot(aes(date, real_spending, color=SeriesLabel)) +
  geom_line() +
  geom_line(aes(date, projection), linetype = "dashed")


  
custom_index <- pce %>%
  filter(SeriesLabel %in% comparison_array) %>%     # keep only the two goods
  group_by(date) %>%
  mutate(
    # Compute the weighted average of the inflation index.
    composite_index = weighted.mean(inflation, PCEweight)
  ) %>%
  ungroup() %>%
  arrange(date) %>%
  # Set the index relative to the first period (base = 100)
  mutate(custom_index = composite_index / composite_index[date == "2019-12-01"] * 100)


custom_index <- custom_index %>% distinct(date, .keep_all = TRUE) %>% select(date, custom_index)
  
custom_index$projection <- proj_log_linear_single(custom_index, "2017-03-01", "2019-12-01") %>% pull()
custom_index %>%
  ggplot(aes(date, custom_index)) + geom_line() +
  geom_line(aes(date, projection), linetype = "dashed")


