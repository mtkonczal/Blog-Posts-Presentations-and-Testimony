library(tidyverse)
library(govMacroTools)
library(broom)
library(tidytext) 
library(govMacroTools)
library(lubridate)
library(scales)
library(glue)
library(slider)
library(gt)

jolts <- getBLSFiles("jolts", "rortybomb@gmail.com")

ces <- getBLSFiles("ces", "rortybomb@gmail.com")