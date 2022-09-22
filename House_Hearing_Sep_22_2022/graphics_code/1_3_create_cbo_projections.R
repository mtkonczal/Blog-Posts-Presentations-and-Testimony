# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

library(tidyverse)
library(ggtext)
library(ggrepel)

##### SET UP SOME THINGS #####
source(file = "graphics_code/1_1_load_bls_cps_jobs_data.R")
source(file = "graphics_code/1_2_load_bls_ces_jobs_data.R")

cbop <- read_csv("data/CBO_prepandemic_labor_projections.csv") %>% #filter(series_title != "(Seas) Employment Level") %>%
  filter(date <= "2022-08-01")

cboa <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(date >= "2021-01-01") %>%
  filter(series_id %in% c("LNS11300000","LNS12000000", "LNS14000000")) %>%
  select(date, value, series_title) %>% mutate(source = "Actual")
maxDate <- max(cboa$date)

cboe <- ces_data %>% filter(series_id %in% "CES0000000001") %>% filter(date >= "2021-01-01") %>%
  select(date, value, series_title) %>% mutate(source = "Actual")
  
cbot <- rbind(cboa, cboe, cbop)
cbot$series_title <- str_replace_all(cbot$series_title, "\\(Seas\\) ", fixed(""))
cbot$series_title <- str_replace_all(cbot$series_title, "\\(Seas\\) ", fixed(""))
cbot$series_title <- str_replace_all(cbot$series_title, "Employment Level", "Employment (Household, Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "Employment \\(CES\\)", "Employment (Establishment, Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "All employees, thousands, total nonfarm, seasonally adjusted", "Employment (Establishment, Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "Civilian Labor Force Level", "Employment (Household, Thousands)")
cbot$source <- str_replace_all(cbot$source, "CBO Projections", "CBO Projections, February 2021")
cbot$source <- str_replace_all(cbot$source, "Actual", "Actual Values")


cbot <- cbot %>% mutate(display_valueAll = value*(date == maxDate))
cbot <- cbot %>% mutate(display_valueE = value*(date == maxDate)*(series_title == "Employment Level (Establishment, in Thousands)")) %>% mutate(display_valueE = floor(display_valueE))
cbot <- cbot %>% mutate(display_valueEH = value*(date == maxDate)*(series_title == "Employment Level (Household, in Thousands)")) %>% mutate(display_valueEH = floor(display_valueEH))
cbot <- cbot %>% mutate(display_valueLFP = value*(date == maxDate)*(series_title == "Labor Force Participation Rate"))
cbot <- cbot %>% mutate(display_valueU = value*(date == maxDate)*(series_title == "Unemployment Rate"))
cbot$display_valueAll <- na_if(cbot$display_valueAll, 0)
cbot$display_valueE <- na_if(cbot$display_valueE, 0)
cbot$display_valueEH <- na_if(cbot$display_valueE, 0)
cbot$display_valueLFP <- na_if(cbot$display_valueLFP, 0)
cbot$display_valueU <- na_if(cbot$display_valueU, 0)

cbo_plot <- ggplot(cbot, aes(date, value, color=source, linetype=source)) + geom_line(size=1.2) + facet_wrap(facets = "series_title", scales = "free") +
  theme_classic() +
  theme(legend.position='none') +
  scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("#01579B", "darkred")) +
  scale_linetype_manual(values = c("solid","dashed")) +
  labs(title="",
       subtitle="",
       caption="",
       x="", y="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  geom_label_repel(aes(x=date, y=display_valueAll, label=round(display_valueAll,1)), box.padding = unit(0.2,"in"))