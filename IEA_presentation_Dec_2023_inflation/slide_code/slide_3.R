library(quantmod)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)

  graphic_title <- "Graphic 1: Deacceleration in Headline PCE"
  unlevel <- prep_FRED_data("UNEMPLOY")
  unrate <- prep_FRED_data("CLF16OV") %>% left_join(unlevel, by="date") %>%
    mutate(unrate = unemploy/clf16ov,
           year = year(date)) %>%
    group_by(year) %>%
    mutate(unrateA = mean(unrate)) %>%
    ungroup()
  rm(unlevel)
  
  #NOT ROBUST TO LATEST DATE
  pce_2023 <- prep_FRED_data("PCEPI")  %>% rename(value = pcepi) %>%
    filter(date >= "2022-12-01") %>%
    mutate(YoY = value[date == max(date)]/ value[date == min(date)],
           YoY = YoY^(12/10) -1) %>%
    filter( date == max(date))
  long_core_pce <- prep_FRED_data("DPCERG3A086NBEA")  %>% rename(value = dpcerg3a086nbea) %>%
    mutate(YoY = value/lag(value,1) - 1)
  month(long_core_pce$date) = 12
  
  long_core_pce <- rbind(long_core_pce, pce_2023)
  long_data <- long_core_pce %>%
    mutate(YoYD = YoY - lag(YoY,1)) %>%
    select(date, YoY, YoYD) %>%
    na.omit() %>%
    left_join(unrate, by="date") %>%
    na.omit() %>%
    mutate(max_value = date != max(date),
           max_value = as.factor(max_value),
           name_value = if_else(YoYD <= YoYD[year(date) == 1981], as.character(format(date, '%Y')), as.character(NA)))
  
  subtitle <- paste0("Change in year-over-year headline PCE from 1 year ago, vs unemployment rate, change < 0, from ", format(min(long_data$date), "%Y"), " to ", format(max(long_data$date), "%Y"), ". 2023 annualized change within 2023.")
  
  
slide3 <- long_data %>%
    filter(YoYD <= 0) %>%
    ggplot(aes(unrateA, YoYD, color=max_value, label=name_value)) +
    theme_classic() +
    geom_point(size = 4) +
  theme(text = element_text(size = 20)) +
    geom_text_repel(size = 10) +
    #geom_line(aes(unrate, predicted), color="#FC8D62") +
  labs(subtitle = "Change in year-over-year headline PCE growth from one year ago, 1948-2023",
       x = "Unemployment Rate",
       y = "") +
    scale_x_continuous(label=percent) +
    scale_y_continuous(label=percent) +
    scale_color_brewer(palette="Set1") +
    theme(legend.position = "none",
          plot.title.position = "plot")



slide3 <- slide3 +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)