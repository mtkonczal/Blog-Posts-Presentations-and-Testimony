library(tidyverse)
library(tidyusmacro)
library(govMacroTools)

cpi <- getBLSFiles("cpi", "konczal@gmail.com")

cpi %>%
  filter(seasonal == "S") %>%
  select(date, item_name, item_display_level, value) %>%
  write_csv("data/cpi_data.csv")

pce <- getPCEInflation(frequency = "M")

pce %>%
  select(date, value = Value, PCEweight, quantity, item_name = SeriesLabel) %>%
  write_csv("data/pce_data.csv")
