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
        axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(size = 20, face="bold"),
        plot.subtitle = element_text(size=12, color="white"), plot.caption = element_text(size=10, face="italic"),
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
cpi$date <- paste(substr(cpi$period, 2,3), "01", cpi$year, sep="/")
cpi$date <- as.Date(cpi$date, "%m/%d/%Y")

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
  mutate(category = fct_relevel(category, "Services")) %>%
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
       title = "Price Increases Are Higher For Services Than Goods",
       subtitle = "Price increases from Jan 2000 through Feb 2023, Lowest level item categories, CPI.",
       caption ="BLS, CPI, seasonally adjusted values only. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5))

ggsave("price_increases.png", dpi="retina", width = 8, height=12, units = "in")

write.csv(AEI_chart, "AEI_chart.csv")

#### All Prices since 2012 Chart ####
since_2012_csv <- cpi %>% filter(seasonal == "S") %>%
  filter(begin_year <= 2012 & end_year == 2023) %>%
  group_by(item_name) %>%
  summarize(change_since_jan_2012 = value[date=="2023-02-01"]/value[date=="2012-01-01"]-1) %>%
  ungroup() %>%
  arrange(desc(change_since_jan_2012))

write.csv(since_2012_csv, "all_since_2012.csv")



##### Final Graphic ####

### Download ECI ###
eci_wages <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.data.1.AllData") %>%
  clean_names()
eci_wages$value <- as.numeric(eci_wages$value)
eci_wages <- eci_wages %>%
  mutate(month = case_when(
    period == "Q01" ~ 3,
    period == "Q02" ~ 6,
    period == "Q03" ~ 9,
    period == "Q04" ~ 12))
eci_wages$date <- paste(eci_wages$month, "01", eci_wages$year, sep="/")
eci_wages$date <- as.Date(eci_wages$date, "%m/%d/%Y")

eci_wages <- eci_wages %>% filter(series_id == "CIS2020000000000I")  %>% mutate(wage_change = value/lag(value)-1) %>% select(wage_change, date)

eci_cpi <- cpi %>% filter(item_name %in% c("Services less energy services","Commodities less food and energy commodities")) %>%
  filter(seasonal == "S") %>% group_by(item_name) %>% mutate(inflation_change = value/lag(value,3)-1) %>%
  ungroup() %>% select(item_name, date, inflation_change) %>%
  inner_join(eci_wages, by="date")


goods <- eci_cpi %>% filter(item_name == "Commodities less food and energy commodities") %>% mutate(wage_change = lag(wage_change,2))
a <- lm(inflation_change ~ wage_change, data=goods)
summary(a)

services <- eci_cpi %>% filter(item_name == "Services less energy services") %>% mutate(wage_change = lag(wage_change,2))
b <- lm(inflation_change ~ wage_change, data=services)
summary(b)

ggplot(eci_cpi, aes(x=lag(wage_change,2), y=inflation_change, color=item_name)) + geom_point() + geom_smooth(method="lm") +
  facet_wrap(~item_name) + theme_lass +
  labs(title="Wages Has a Positive, Significant Relationship with Services Inflation, But Not Goods",
       subtitle="3-month change in ECI private wages, CPI inflation category, wages lagged 6 months",
       caption="BLS, CPI, ECI, seasonally-adjusted. Same effect with or without lag. Author's calculations, Mike Konczal",
       y="CPI Inflation, 3-months", x="ECI Private Wages, 3-Months, Lagged 6 Months") +
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent) +
  theme(axis.title.x = element_text(color="white"), axis.title.y = element_text(color="white", angle=90))
  
ggsave("wages_goods_services.png", dpi="retina", width = 12, height=8, units = "in")