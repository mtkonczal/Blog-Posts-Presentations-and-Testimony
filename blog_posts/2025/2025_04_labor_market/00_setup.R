library(tidyverse)
library(govMacroTools)
# gov macro tools available here:
# https://github.com/mtkonczal/govMacroTools
library(gt)
library(broom)
library(lubridate)
library(scales)


ces <- getBLSFiles("ces", "konczal@gmail.com")
