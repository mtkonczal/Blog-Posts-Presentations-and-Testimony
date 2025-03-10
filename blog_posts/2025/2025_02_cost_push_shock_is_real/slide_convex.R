library(lubridate)
library(ggrepel)
library(scales)
library(tidyverse)
library(govMacroTools)

convex_curve_data <- read_csv("convex_data.csv")


cs <- getPCEInflation(frequency = "Q")

cs <- cs %>% filter(SeriesCode %in% c("DGDSRG","DSERRG")) %>%
  mutate(date_label = format(date, "%b\n%Y"))

cs %>%
  filter(year(date) >= "2018-12-01") %>%
  filter(year(date) <= 2022) %>%
  mutate(cs_label = if_else(month(date)==12, date_label, NA)) %>%
  ggplot(aes(quantity, Value, label = cs_label)) +
  facet_wrap(~SeriesLabel, scales = "free") +
  geom_path(size=1.2) +
  geom_point(size=1.2) +
  geom_label() +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  labs(
    title = "Services Spending Collapses During the Pandemic But Its Price Level Does Not - A Cost-Push Shock.", y = NULL,
    x = "Quantity (Index, 2017=100)", subtitle = "Price (Index, 2017=100)",
    caption = "Quarterly PCE Values, Tables 2.4.3 and 2.4.5. Mike Konczal"
  ) +
#  ylim(95,120) +
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




There's 2 problems with the quote here:
- Goods jump but services fall and only barely recover. There's no overall increased appetite for (real PCE) spending.

- While services fall, service prices don't, and then they recover into the higher price level. This is a classic cost-push shock scenario.