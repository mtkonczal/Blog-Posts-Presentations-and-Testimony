library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)


beaKey <- ENTER HERE
source("graphics_code/2_1_BEA_helper_functions.R")
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

# a)	Consumption is not above trend (above what we would expect given incomes—not based on “excess” cash balances
# b)	Cash balances have not been depleted rapidly—except for making unusual tax payments (dean baker)
# c)	Investment in plant and equipment is in fact below trend
# d)	Real government expenditures (not transfer payments) are not significantly above trend
# e)	Net exports have actually decreased (?), and would have decreased even more had there not be supply side bottlenecks in China
# f)	GDP has been weak—and would be even weaker if there had not been unintended inventory accumulation

#### SAME GRAPHIC BUT GREAT RECESSION

##### # a)	Consumption is not above trend (above what we would expect given incomes—not based on “excess” cash balances
BEA_a_GR <- get_NIPA_data(beaKey, 'T20303', 'Q', '2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022')
BEA_a_GR <- BEA_date_quarterly(BEA_a_GR)

BEA_a_GR <- draw_Trendline(BEA_a_GR, "12/1/2005", "12/1/2007", 3)
BEA_a_GR <- BEA_a_GR %>% rename(trendline2 = trendline)
BEA_a_GR <- draw_Trendline(BEA_a_GR, "12/1/2017", "12/1/2019", 3)

bea_trend <- BEA_a_GR %>% filter(SeriesCode == "DPCERA") %>%
  ggplot(aes(date, DataValue)) + geom_line(size=1.1) +
  theme_classic() + geom_line(aes(date,trendline2, color="red"), size=1, linetype="longdash") +
  geom_line(aes(date,trendline, color="red"), size=1, linetype="longdash") +
  labs(y = NULL, x = NULL, title="",
       subtitle="",
       caption = "") +
  scale_x_date(date_labels = "%Y") +
  theme(legend.position = "none")

