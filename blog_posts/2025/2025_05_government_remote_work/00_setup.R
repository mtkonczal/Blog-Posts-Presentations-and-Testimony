library(govMacroTools)
library(lubridate)
library(scales)
library(tidyverse)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

# This is from govMacroTools, a personal library:
# https://github.com/mtkonczal/govMacroTools
cps <- getBLSFiles("cps", "rortybomb@gmail.com")

cpsS <- cps %>% filter(seasonal == "S")
MI_dates <- sort(unique(cpsS$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 6)]