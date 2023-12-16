library(lubridate)
library(hrbrthemes)
library(ggrepel)
library(ggridges)
library(scales)
library(quantmod)
library(tidyverse)

convex_curve_data <- read_csv("data/convex_data.csv")

convex_curve_goods <- convex_curve_data %>%
  filter(LineDescription == "Goods") %>%
  ggplot(aes(Quantity, Price, label = TimePeriod_a)) +
  geom_point(color="#FFBF00", size=2) +
  geom_path(color="#FFBF00", size=1.2) +
  theme_classic() +
  geom_label(size = 6) +
  theme(text = element_text(size = 20)) +
  labs(
    title = NULL, y = NULL,
    x = "Quantity (Index, 2012=100)", subtitle = "Price (Index, 2012=100)",
    caption = NULL
  ) +
  annotate("text", x=120, y= 110,
           label = "Goods", size = 8, color = "#FFBF00") +
  ylim(95,120) +
  theme(plot.title.position = "plot")

convex_curve_goods <- convex_curve_goods +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)


convex_curve_services <- convex_curve_data %>%
  filter(LineDescription == "Services") %>%
  ggplot(aes(Quantity, Price, label = TimePeriod_a)) +
  geom_point(color="#2D779C", size=2) +
  geom_path(color="#2D779C", size=1.2) +
  theme_classic() +
  geom_label(size = 6) +
  theme(text = element_text(size = 20)) +
  labs(
    title = NULL, y = NULL,
    x = "Quantity (Index, 2012=100)", subtitle = "Price (Index, 2012=100)",
    caption = NULL
  ) +
  annotate("text", x=100, y= 113,
           label = "Services", size = 8, color = "#2D779C") +
  ylim(95, 130) +
  theme(plot.title.position = "plot")

convex_curve_services <- convex_curve_services +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)