library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(janitor)
library(ggrepel)
library(quantmod)
library(tidyusmacro)

pce <- getPCEInflation(frequency = "M") %>%
  mutate(LineDescription = SeriesLabel, DataValue = Value)

write_rds(pce, "data/pce_monthly.rds")