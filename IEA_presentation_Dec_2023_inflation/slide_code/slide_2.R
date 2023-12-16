library(quantmod)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)

  unlevel <- prep_FRED_data("UNEMPLOY")
  unrate <- prep_FRED_data("CLF16OV") %>% left_join(unlevel, by="date") %>%
    mutate(unrate = unemploy/clf16ov)
  rm(unlevel)
  
  long_core_pce <- prep_FRED_data("PCEPILFE")  %>% rename(value = pcepilfe)
  
  long_data <- long_core_pce %>%
    mutate(YoY = value/lag(value,12) - 1,
           YoYD = YoY - lag(YoY,12)) %>%
    select(date, YoY, YoYD) %>%
    na.omit() %>%
    left_join(unrate, by="date") %>%
    mutate(max_value = date < max(date) %m-% months(6),
           max_value = as.factor(max_value),
           name_value = if_else(YoYD < YoYD[date == max(date)] | date > max(date) %m-% months(3), as.character(format(date, '%b\n%Y')), as.character(NA)))
  
  slide2 <- long_data %>%
    filter(YoYD <= 0) %>%
    ggplot(aes(unrate, YoYD, color=max_value, label=name_value)) +
    theme_classic() +
    geom_point() +
    theme(text = element_text(size = 20)) +
    geom_text_repel(size = 6) +
    labs(subtitle = "Change in year-over-year core PCE growth from one year ago, 1961-2023",
         x = "Unemployment Rate",
         y = "") +
    scale_x_continuous(label=percent) +
    scale_y_continuous(label=percent) +
    scale_color_brewer(palette="Set1") +
    theme(legend.position = "none") +
    theme(plot.title.position = "plot")

  
  
  slide2 <- slide2 +  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )