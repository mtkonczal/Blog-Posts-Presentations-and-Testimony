library(lubridate)
library(hrbrthemes)
library(ggrepel)
library(ggridges)
library(scales)
library(quantmod)
library(tidyverse)

normalized <- read_csv("data/normalized.csv")

date_breaks <- normalized$date
date_breaks <- sort(unique(date_breaks), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 12)]

graphic_shift <- normalized %>%
  ggplot(aes(date, DataValue, color = LineDescription)) +
  theme_classic() +
  geom_line(size = 1.2) +
  geom_line(aes(date, trendline), size = 0.6, linetype = "longdash") +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    subtitle = "Real personal consumption expenditures, January 2020 = 1",
    caption = NULL
  ) +
  scale_color_manual(values=c("#FFBF00","#2D779C")) +
  theme(legend.position = "none",
        plot.title.position = "plot") +
  annotate("text", x=as.Date("2019-01-01"), y= 0.94,
           label = "Services", size = 8, color = "#2D779C") +
  annotate("text", x=as.Date("2019-01-01"), y= 1.02,
           label = "Goods", size = 8, color = "#FFBF00") +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks)




graphic_shift <- graphic_shift +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)