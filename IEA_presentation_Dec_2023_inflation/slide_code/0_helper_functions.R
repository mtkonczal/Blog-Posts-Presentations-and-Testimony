library(quantmod)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)

prep_FRED_data <- function(x) {
  getSymbols(x, src="FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}
