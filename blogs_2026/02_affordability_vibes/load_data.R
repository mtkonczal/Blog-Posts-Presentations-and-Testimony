library(tidyverse)
library(tidyusmacro)


cpi <- getBLSFiles("cpi", "konczal@gmail.com")

cpi %>%
  filter(seasonal == "S") %>%
  select(date, item_name, item_display_level, value) %>%
  write_csv("cpi_data.csv")

pce <- tidyusmacro::getPCEInflation(frequency = "M")

pce %>% select(date, value = Value, PCEweight, quantity, item_name = SeriesLabel) %>%
  write_csv("pce_data.csv")