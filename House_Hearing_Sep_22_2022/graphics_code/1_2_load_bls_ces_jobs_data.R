###############################################################
# Code to read in jobs data from BLS CES website and begin analysis.
# This file reads in and store all the Establishment Survey monthly jobs data.
#
# Mike Konczal
# Last updated 3/31/22

library(janitor)
library(tidyverse)
library(ggtext)

#### CES EMPLOYMENT ########

ces_data <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.data.01a.CurrentSeasAE")
# ce.data.0.AllCESSeries has ALL the data.

ces_data <- ces_data %>%
  clean_names()
# Right now R doesn't handle dates before 1970 straightforward, so as a workaround,
# and since we don't need them, I'm just deleting them. Will fix in future version.
ces_data$series_id <- str_trim(ces_data$series_id)
ces_data$value <- as.numeric(ces_data$value)
ces_data$date <- paste(substr(ces_data$period, 2,3), "01", ces_data$year, sep="/")
ces_data$date <- as.Date(ces_data$date, "%m/%d/%Y")

ces_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.series")
ces_series <- ces_series %>% 
  clean_names()
ces_series$series_id <- str_trim(ces_series$series_id)

ces_data_type <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.datatype")
ces_super_sector <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.supersector")
ces_series <- inner_join(ces_series, ces_data_type, by = "data_type_code")
ces_series <- inner_join(ces_series, ces_super_sector, by = "supersector_code")

ces_data <- inner_join(ces_data, ces_series, by = c("series_id"))
ces_data <- select(ces_data, -c("footnote_codes.x", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period"))


#save(ces_data, file = "data/ces_data.RData")
######################################################################