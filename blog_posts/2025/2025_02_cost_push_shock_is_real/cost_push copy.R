library(tidyverse)
library(govMacroTools)
library(lubridate)


ces <- getBLSFiles("ces", "rortybomb@gmail.com")

total_jobs_df <- ces %>% filter(series_id == "CES0000000001") %>% select(date, total_jobs = value)

jobs <- ces %>%
  filter(seasonal == "S", data_type_code == "01") %>%
  filter(date == "2019-12-01" | date == "2024-12-01") 



df <- jobs %>%
  filter(display_level == 5) %>%
  left_join(total_jobs_df, by="date") %>%
  group_by(industry_name) %>%
  reframe(before = value[date == min(date)]/total_jobs[date == min(date)],
          after = value[date == max(date)]/total_jobs[date == max(date)],
          diff = after - before,
          supersector_name = supersector_name[date == min(date)]) %>%
  ungroup()


#### The percent of workers in leisure and hospitality - useful graphic #####
ces %>%
  group_by(date) %>%
  reframe(per = value[series_id == "CES7000000001"]/value[series_id == "CES0000000001"]) %>%
  ungroup() %>%
  ggplot(aes(date, per)) + geom_line()
#####

ces %>% filter(supersector_code == "00", industry_code == "00000000", seasonal == "S", date == "2024-01-01")

wages <- ces %>% filter(seasonal == "S", data_type_code == "03")

# Merge wages in and see.
wages %>%
  filter(date == "2024-01-01") %>%
  group_by(display_level) %>%
  reframe(n = n())


# Wages
wages <- ces %>% filter(seasonal == "S", data_type_code == "03", date == "2024-12-01" | date == "2019-12-01") %>% select(date, industry_code, wages = value)
jobs <-  ces %>% filter(seasonal == "S", data_type_code == "01", date == "2024-12-01" | date == "2019-12-01") %>% select(date, industry_code, display_level, jobs = value)
together <- inner_join(wages, jobs, by=c("industry_code", "date")) %>% left_join(total_jobs_df, by="date")

rbind(wages, jobs) %>%
  group_by(display_level, data_type_text) %>%
  reframe(n = n())