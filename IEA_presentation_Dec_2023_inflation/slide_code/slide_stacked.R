library(lubridate)
library(hrbrthemes)
library(ggrepel)
library(ggridges)
library(scales)
library(quantmod)
library(tidyverse)


goods <- prep_FRED_data("CUSR0000SACL1E") %>%
  rename(core_goods = cusr0000sacl1e) %>%
  mutate(core_goods = core_goods/lag(core_goods,1),
         core_goods = core_goods^12-1,
         core_goods = core_goods*21.361/100)


services <- prep_FRED_data("CUSR0000SASLE") %>%
  rename(core_services = cusr0000sasle) %>%
  mutate(core_services = core_services/lag(core_services,1),
         core_services = core_services^12-1,
         core_services = core_services*58.187/100)

date_breaks <- services %>% filter(month(date) == 1) %>% select(date) %>% pull()


stacked_chart <- services %>%
  left_join(goods, by="date") %>%
  na.omit() %>%
  pivot_longer(core_services:core_goods, names_to = "type", values_to = "values") %>%
    filter(date >= "2019-01-01", values > -0.018) %>%
    mutate(num_label = round(100*values, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
    ggplot(aes(x = date, y = values, fill = type, label = num_label)) +
    geom_bar(stat = 'identity', size=0) +
  theme_classic() +
    labs(y = NULL,
         x = NULL,
         subtitle = "Monthly contribution to CPI inflation") +
  scale_fill_manual(values=c("#FFBF00","#2D779C")) +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  theme(legend.position = "none",
        plot.title.position = "plot") +
  theme(text = element_text(size = 20)) +
  geom_vline(xintercept = as.Date("2021-01-01")) +
  geom_vline(xintercept = as.Date("2022-01-01")) +
  geom_vline(xintercept = as.Date("2023-01-01")) +
  annotate("text", x=as.Date("2019-06-01"), y= 0.03,
           label = "Core Services", size = 8, color = "#2D779C") +
  annotate("text", x=as.Date("2019-06-01"), y= -0.01,
           label = "Core Goods", size = 8, color = "#FFBF00") +
  annotate("text", x=as.Date("2021-06-01"), y= 0.10,
           label = "Transitory", size = 6) +
  annotate("text", x=as.Date("2022-06-01"), y= 0.10,
           label = "Persistent", size = 6) +
  annotate("text", x=as.Date("2023-06-01"), y= 0.10,
           label = "Disinflation", size = 6)


stacked_chart <- stacked_chart +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent', color=0),
  legend.box.background = element_rect(fill='transparent')
)