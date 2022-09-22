###############################################################
# Code to read in jobs data from CPS website and begin analysis.
# This file reads in and store all the CPS monthly jobs data.
# This file handles employment.
#
# Mike Konczal
# Last updated 3/30/22
library(janitor)
library(tidyverse)
library(ggtext)


#### EMPLOYMENT ########

cps_jobs_data <- read_delim(file = "https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData")


cps_jobs_data <- cps_jobs_data %>%
  clean_names()
# Right now R doesn't handle dates before 1970 straightforward, so as a workaround,
# and since we don't need them, I'm just deleting them. Will fix in future version.
cps_jobs_data <- filter(cps_jobs_data, period != "M13")
cps_jobs_data$series_id <- str_trim(cps_jobs_data$series_id)
cps_jobs_data$value <- as.numeric(cps_jobs_data$value)
cps_jobs_data$date <- paste(substr(cps_jobs_data$period, 2,3), "01", cps_jobs_data$year, sep="/")
cps_jobs_data$date <- as.Date(cps_jobs_data$date, "%m/%d/%Y")

cps_jobs_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ln/ln.series")
cps_jobs_series <- cps_jobs_series %>% 
  clean_names()
cps_jobs_series$series_id <- str_trim(cps_jobs_series$series_id)

cps_ages <- read_delim(file = "https://download.bls.gov/pub/time.series/ln/ln.ages")
cps_occupation <- read_delim(file = "https://download.bls.gov/pub/time.series/ln/ln.occupation")
cps_jobs_series <- inner_join(cps_jobs_series, cps_ages, by = "ages_code")
cps_jobs_series <- inner_join(cps_jobs_series, cps_occupation, by = "occupation_code")

cps_jobs_data <- inner_join(cps_jobs_data, cps_jobs_series, by = "series_id") %>%
  select(-c("footnote_codes.x", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period"))

###### END DATA IMPORT #######
