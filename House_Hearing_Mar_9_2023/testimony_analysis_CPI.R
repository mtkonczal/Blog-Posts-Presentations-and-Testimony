###############################################################
# Graphics that I'm watching, Early 2023, inflation
# Mike Konczal
# Last updated 2/13/2023

library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)
library(tidytext)
library(viridis)
library(ggridges)

#Either run Script 1 to download data fresh, or used locally stored data.
source(file = "/Users/mkonczal/Documents/GitHub/Blog-Posts-Presentations-and-Testimony/House_Hearing_Mar_9_2023/1_load_cpi_data.R")

cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value, 1)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Pchange3a = (1 + Pchange3)^4 - 1) %>%
  mutate(Pchange6a = (1 + Pchange6)^2 - 1) %>%
  mutate(YoY = (value/lag(value,12) - 1)) %>%
  mutate(TwoYear = value/lag(value,24)-1) %>%
  mutate(TwoYearA = (1+TwoYear)^(1/2)-1) %>%
  ungroup()


# TABLE 1
headline_categories <- c("All items","All items less food and energy","All items less food, shelter, energy, and used cars and trucks")

inflation_declining <- cpi %>% filter(item_name %in% c(headline_categories)) %>%
  select(date, item_name, Pchange3a, Pchange6a, YoY) %>%
  group_by(item_name) %>%
  mutate(June2022_values = Pchange6a[date=="2022-06-01"]) %>%
  mutate(Values_2021 = YoY[date=="2021-12-01"]) %>%
  ungroup() %>%
  filter(date == max(date))

inflation_declining <- inflation_declining[-1,] %>%
  mutate(Pchange3a = percent(Pchange3a), Pchange6a = percent(Pchange6a), June2022_values = percent(June2022_values)) %>%
  mutate(item_name = str_replace_all(item_name, "All items less food, shelter, energy, and used cars and trucks","Core inflation less shelter and used cars")) %>%
  arrange(item_name)

inflation_declining_table <- inflation_declining %>%
  select(`Category Name, CPI` = item_name, `1st Half, 2022` = June2022_values, `2nd Half, 2022` = Pchange6a, `Last 3 Months` = Pchange3a)


age_distribution <- read_csv("data/age_distribution.csv")

age_distribution_graphic <- ggplot(age_distribution, aes(AGE, employment_rate, color=as.factor(YEAR))) +
  geom_line(size=1) + theme_classic() +
  theme(legend.position = "bottom") +
  labs(x=NULL, y=NULL,
       subtitle=NULL,
       title=NULL,
       caption=NULL) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = 5*(1:18)) +
  theme(legend.position = c(0.3,0.5), legend.title = element_blank())

#######

merged <- read_csv("data/merged.csv")
merged_prior <- merged %>% filter(date < "2021-01-01")


a <- lm(ECI_growth ~ quitsR, data=merged_prior)
summary(a)


merged_graphic <- merged %>%
  mutate(values_last = if_else(date >= "2022-01-01", date, as.Date(NA))) %>% mutate(values_last2 = as.character(format(values_last, "%b\n%Y"))) %>%
  mutate(Is_2021_to_2022 = (date >= "2021-01-01")) %>%
  mutate(Is_2021_to_2022_v = if_else(date >= "2022-01-01",ECI_growth,as.numeric(NA))) %>%
  ggplot(aes(quitsR, ECI_growth, color=Is_2021_to_2022, label=values_last2)) + geom_point() + theme_classic() +
  geom_abline(intercept = a$coefficients[1], slope=a$coefficients[2], color="red") +
  geom_text_repel(size=2) +
  geom_path(aes(quitsR,Is_2021_to_2022_v)) +
  labs(x = "Quit Rate",
       y = "ECI Private Wage Growth, Quarterly",
       title = NULL,
       subtitle = NULL,
       caption =NULL) +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot", legend.position="none") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  scale_fill_brewer(palette="Paired")

