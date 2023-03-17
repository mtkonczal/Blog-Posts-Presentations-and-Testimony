# Doing the AEI Inflation Chart, But With All Categories
# Written by: Mike Konczal
# Last Updated: 3-17-2023
library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(tidytext)
library(lubridate)
##### SET UP SOME THINGS #####

##### SET UP CUSTOM THEME #####
# If there's an error in the code, it's probably because you don't have font family Larsseit installed
theme_lass <-   theme_modern_rc(ticks = TRUE) +
  theme(legend.position = "none", legend.title = element_blank(),
        panel.grid.major.y = element_line(size=0.5),panel.grid.minor.y = element_blank(), plot.title.position = "plot",
        axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(size = 25, face="bold"),
        plot.subtitle = element_text(size=15, color="white"), plot.caption = element_text(size=10, face="italic"),
        legend.text = element_text(size=12), axis.text.y = element_text(size=12, face="bold"), axis.text.x = element_text(size=12, face="bold"),
        strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10), panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"), plot.title = element_text(family = "Larsseit"), plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"), strip.text = element_text(family="Larsseit"))

#### Load CPI data ####
cpi <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.data.0.Current")
cpi <- cpi %>%
  clean_names()
cpi$value <- as.numeric(cpi$value)
cpi$series_id <- str_trim(cpi$series_id)
cpi$date <- paste(substr(cpi$period, 2,3), "01", substr(cpi$year, 3, 4), sep="/")
cpi$date <- as.Date(cpi$date, "%m/%d/%y")

series <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.series")
series <- series %>%
  clean_names()
series$series_id <- str_trim(series$series_id)

items <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.item")
series <- inner_join(series, items, by = c("item_code"))

cpi <- inner_join(cpi, series, by = c("series_id"))

##### AEI chart ####
lowest_level_prices <- read_csv("weights/most_prices.csv") %>% filter(lowest == 1 | item_name == "Food away from home")
lowest_level_prices <- lowest_level_prices %>% select(item_name, category)

AEI_chart <- cpi %>% filter(seasonal == "S", item_name %in% lowest_level_prices$item_name) %>%
  left_join(lowest_level_prices, by=c("item_name")) %>% filter((category %in% c("Goods","Services")) | item_name == "Food away from home")

AEI_chart <- AEI_chart %>%
  filter(begin_year <= 2000 & end_year == 2023) %>%
  group_by(item_name, category) %>%
  summarize(change = value[date=="2023-02-01"]/value[date=="2000-01-01"]-1) %>%
  ungroup() %>%
  arrange(desc(change))

AEI_chart <- AEI_chart %>%
  mutate(item_name = str_replace_all(item_name, "Club membership for shopping clubs, fraternal, or other organizations, or participant sports fees","Club memberships")) %>%
  mutate(item_name = str_replace_all(item_name, "Other lodging away from home including hotels and motels","Other lodging")) %>%
  mutate(category = str_replace_all(category, "Food","Services"))

AEI_chart %>%
  mutate(category = as.factor(category), name = reorder_within(item_name, change, category))  %>%
  ggplot(aes(name, change, fill = category)) +
  geom_col(size=0) +
  coord_flip() +
  scale_x_reordered() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "Price Increases",
       subtitle = "Price increase since 2000.",
       caption ="BLS, CPI, Author's Calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5))

ggsave("price_increases.png", dpi="retina", width = 8, height=12, units = "in")

write.csv(AEI_chart, "AEI_chart.csv")

#### All Prices since 2013 Chart ####
since_2013_csv <- cpi %>% filter(seasonal == "S") %>%
  filter(begin_year <= 2012 & end_year == 2023) %>%
  group_by(item_name) %>%
  summarize(change = value[date=="2023-02-01"]/value[date=="2012-01-01"]-1) %>%
  ungroup() %>%
  arrange(desc(change))

write.csv(since_2013_csv, "all_since_2013.csv")

