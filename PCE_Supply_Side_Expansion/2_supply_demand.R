library(quantmod)
library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)

source("0_a_helper_functions.R")
source("0_b_load_PCE_items.R")
#load("data/pce_supply_demand.RData")
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

#"Such an aggregation leaves 136 categories in the PCE price index and 124
#categories in the core PCE index."
# Function to remove outliers for graphics
remove_outliers <- function(x, multiplier = 1.5) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - multiplier * IQR
  upper_bound <- Q3 + multiplier * IQR
  x[(x < lower_bound) | (x > upper_bound)] <- NA
  return(x)
}

# Get preloaded categories and levels for each
lowest <- read_csv("data/pce_items_lowest.csv")

months_change <- 6
compare_date <- "2022-12-01"

recent <-pce %>% group_by(date) %>%
  distinct(LineNumber, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(LineDescription) %>%
  mutate(QuantityFinal = Quantity/lag(Quantity,months_change)-1,
         PriceFinal = DataValue/lag(DataValue,months_change)-1,
  ) %>%
  filter(date == max(date) | date == compare_date) %>%
  summarize(QuantityFinal = QuantityFinal[date == max(date)] - QuantityFinal[date == compare_date],
            PriceFinal = PriceFinal[date == max(date)] - PriceFinal[date == compare_date],
            weight = PCEweight[date == max(date)]) %>%
  ungroup()

pce_supply <- recent %>% inner_join(lowest,by="LineDescription") %>% filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  filter(category != "Food and Energy") %>%
  arrange(LineDescription) %>%
  filter(level == "Level 4") %>%
  mutate(QuantityFinal = remove_outliers(QuantityFinal, 4)) %>%
  mutate(PriceFinal = remove_outliers(PriceFinal, 4)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight, color=category)) + geom_point(aes(fill="skyblue"), alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "3-month Change July 2023 Minus 3-month Change December 2022, Quantity and Inflation, for ~130 Core PCE Item Categories",
       title="Deceleration is Driven by Expanded Supply",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = c(0.85,0.9)) +
  scale_size_continuous(range = c(3, 10)) +
  facet_wrap(~category)

ggsave("graphics/supply_demand.png", dpi="retina", width = 12, height=6.75, units = "in")


quandrants <-
recent %>% inner_join(lowest,by="LineDescription") %>%
  filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  arrange(LineDescription) %>%
  filter(level == "Level 4") %>%
  #left_join(pce_sd, by="LineDescription") %>%
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "1Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "2Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "3Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "4Supply+",
    TRUE ~ "Undefined"))


quandrants_print <- quandrants %>% select(-leading_spaces, -lowest)
quandrants_print$quadrant <- substr(quandrants_print$quadrant, 2, nchar(quandrants_print$quadrant))
write_csv(quandrants_print, "quandrants_data.csv")

quandrants_center <- quandrants %>% left_join(pce_sd, by="LineDescription") %>%
  filter(PriceFinal < 0) %>%
  mutate(quadrant = if_else(abs(QuantityFinal) < 0.001,"center-",quadrant)) %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(Sn = n/sum(n)) %>%
  arrange(category)

quandrants_center <- quandrants %>%
  filter(category != "Food and Energy") %>%
  filter(PriceFinal < 0) %>%
  mutate(quadrant = if_else(abs(QuantityFinal) < 0.001,"center-",quadrant)) %>%
  group_by(quadrant) %>%
  summarize(n = sum(weight)) %>%
  ungroup() %>%
  mutate(Sn = n/sum(n))


all <- quandrants %>%
#execute code %>%
  filter(category != "Food and Energy") %>%
  mutate(category = "All_Core") %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW))

g_s <-  quandrants %>%
  #execute code %>%
  filter(category != "Food and Energy") %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW)) %>%
  ungroup() %>%
  rbind(all)


chart1 <- g_s %>% filter(category != "Food and Energy") %>% select(-n,-nW) %>%
  pivot_wider(names_from = category, values_from = c(Sn,SnW)) %>%
  mutate(fall_all = if_else(SnW_All_Core > 0, SnW_All_Core/(SnW_All_Core[3]+SnW_All_Core[4]),as.numeric(NA)),
         fall_services = if_else(SnW_Goods > 0, SnW_Services/(SnW_Services[3]+SnW_Services[4]),as.numeric(NA)),
         fall_goods = if_else(SnW_Goods > 0, SnW_Goods/(SnW_Goods[3]+SnW_Goods[4]),as.numeric(NA))) %>%
  select(quadrant, Sn_All_Core, SnW_All_Core, fall_all, Sn_Goods, SnW_Goods, fall_goods,Sn_Services, SnW_Services, fall_services)

f_pct <- function(n) {
  return(str_c(sprintf('%.f', 100*n), "%"))
}
chart1$Sn_All_Core <- f_pct(chart1$Sn_All_Core)
chart1$Sn_Goods <- f_pct(chart1$Sn_Goods)
chart1$Sn_Services <- f_pct(chart1$Sn_Services)
chart1$fall_all <- f_pct(chart1$fall_all) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_services <- f_pct(chart1$fall_services) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_goods <- f_pct(chart1$fall_goods) %>% str_replace_all("NA%",as.character(NA))
chart1$SnW_All_Core <- round(chart1$SnW_All_Core,2)
chart1$SnW_Goods <- round(chart1$SnW_Goods,2)
chart1$SnW_Services <- round(chart1$SnW_Services,2)

chart1$quadrant <- substr(chart1$quadrant, 2, nchar(chart1$quadrant))
write_csv(chart1, "chart1.csv")

#### assume we have shapiro loaded  - cyclical_categories ####

cyclical_categories <- c("Accessories and parts",
                         "Veterinary and other services for pets",
                         "Bicycles and accessories",
                         "Child care",
                         "Amusement parks, campgrounds, and related recreational services",
                         "Clothing and footwear services",
                         "Household care services",
                         "Social advocacy and civic and social organizations",
                         "Less: Personal remittances in kind to nonresidents",
                         "Household cleaning products",
                         "Nursing homes (52)",
                         "Package tours",
                         "Labor organization dues",
                         "Museums and libraries",
                         "Lotteries",
                         "Imputed rental of owner-occupied nonfarm housing (21)",
                         "Pari-mutuel net receipts",
                         "Miscellaneous household products",
                         "Group housing (23)",
                         "Pleasure boats, aircraft, and other recreational vehicles",
                         "Admissions to specified spectator amusements",
                         "Social assistance",
                         "Purchased meals and beverages (102)",
                         "Casino gambling",
                         "Religious Organizations' Services to HHs",
                         "Motor vehicle maintenance and repair (60)",
                         "Household paper products",
                         "Domestic services",
                         "Rental of tenant-occupied nonfarm housing (20)",
                         "Final consumption expenditures of nonprofit institutions serving households (NPISHs) (132)")

#summarize cyclical_categories
cyclical_pce <- 
recent %>%
  filter(LineDescription %in% cyclical_categories) %>% select(LineDescription)

as_tibble(cyclical_categories) %>%
  filter(!value %in% recent$LineDescription)

write_csv(cyclical_pce, "cyclical_pce.csv")


recent %>% inner_join(lowest,by="LineDescription") %>%
  filter(LineDescription %in% cyclical_categories)


anti_join(cyclical_categories, recent$LineDescription, b)

recent %>% 
  filter(LineDescription %in% cyclical_categories) %>%
  inner_join(lowest,by="LineDescription") %>%
#  mutate(QuantityFinal = remove_outliers(QuantityFinal, 3)) %>%
#  mutate(PriceFinal = remove_outliers(PriceFinal, 3)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight,fill=category)) + geom_point(alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "3-month Change June 2023 Minus 3-month Change June 2022, Quantity and Inflation, for ~130 Core PCE Item Categories",
       title="Deceleration is Driven by Expanded Supply",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = "bottom") +
  scale_size_continuous(range = c(3, 10))


cyclical_pce %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight,fill=category)) + geom_point(alpha=0.5, shape = 21, color = "black", stroke = 1.5) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "3-month Change June 2023 Minus 3-month Change June 2022, Quantity and Inflation, for ~130 Core PCE Item Categories",
       title="Deceleration is Driven by Expanded Supply",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") + theme(legend.position = c(0.25,0.25)) + guides(size = FALSE) 


quandrants <-
  recent %>% inner_join(lowest,by="LineDescription") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  filter(LineDescription %in% cyclical_categories) %>%
  filter(category != "Food and Energy") %>%
  #left_join(pce_sd, by="LineDescription") %>%
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "1Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "2Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "3Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "4Supply+",
    TRUE ~ "Undefined")) %>%
  group_by(quadrant) %>%
  summarize(nn = n(), n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW)) %>%
  ungroup()


chart2 <- quandrants %>%
  mutate(fall_all = if_else(SnW > 0, SnW/(SnW[3]+SnW[4]),as.numeric(NA))) %>%
  select(quadrant, Sn, SnW, fall_all)

f_pct <- function(n) {
  return(str_c(sprintf('%.f', 100*n), "%"))
}
chart2$Sn <- f_pct(chart2$Sn)
chart2$fall_all <- f_pct(chart2$fall_all) %>% str_replace_all("NA%",as.character(NA))
chart2$SnW <- round(chart2$SnW,2)
chart2$quadrant <- substr(chart2$quadrant, 2, nchar(chart2$quadrant))
write_csv(chart2, "chart2.csv")


chart1 <- g_s %>% filter(category != "Food and Energy") %>% select(-n,-nW) %>%
  pivot_wider(names_from = category, values_from = c(Sn,SnW)) %>%
  mutate(fall_all = if_else(SnW_All_Core > 0, SnW_All_Core/(SnW_All_Core[3]+SnW_All_Core[4]),as.numeric(NA)),
         fall_services = if_else(SnW_Goods > 0, SnW_Services/(SnW_Services[3]+SnW_Services[4]),as.numeric(NA)),
         fall_goods = if_else(SnW_Goods > 0, SnW_Goods/(SnW_Goods[3]+SnW_Goods[4]),as.numeric(NA))) %>%
  select(quadrant, Sn_All_Core, SnW_All_Core, fall_all, Sn_Goods, SnW_Goods, fall_goods,Sn_Services, SnW_Services, fall_services)

f_pct <- function(n) {
  return(str_c(sprintf('%.f', 100*n), "%"))
}
chart1$Sn_All_Core <- f_pct(chart1$Sn_All_Core)
chart1$Sn_Goods <- f_pct(chart1$Sn_Goods)
chart1$Sn_Services <- f_pct(chart1$Sn_Services)
chart1$fall_all <- f_pct(chart1$fall_all) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_services <- f_pct(chart1$fall_services) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_goods <- f_pct(chart1$fall_goods) %>% str_replace_all("NA%",as.character(NA))
chart1$SnW_All_Core <- round(chart1$SnW_All_Core,2)
chart1$SnW_Goods <- round(chart1$SnW_Goods,2)
chart1$SnW_Services <- round(chart1$SnW_Services,2)

chart1$quadrant <- substr(chart1$quadrant, 2, nchar(chart1$quadrant))
write_csv(chart1, "chart1.csv")


c <- b %>% filter(category != "Food and Energy") %>% select(-n,-nW) %>%
  pivot_wider(names_from = category, values_from = c(Sn,SnW))

b %>% group_by(category) %>% summarize(sum(nW))

#### Historical dive ####

pce %>% filter(LineDescription %in% lowest$LineDescription) %>%
  group_by(date) %>%
  summarize(median = median(DataValue_P1),
            p25 = quantile(DataValue_P1, probs = 0.25, na.rm=TRUE),
            p90 = quantile(DataValue_P1, probs = 0.9, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(median:p90, names_to = "type", values_to = "change") %>%
  filter(change > -0.75) %>%
  ggplot(aes(date,change)) + geom_point() + theme_classic() + facet_wrap(~type, scales = "free")


pce %>% filter(LineDescription %in% lowest$LineDescription) %>%
  group_by(LineDescription) %>%
  mutate(DataValue_P6 = DataValue/lag(DataValue,6),
         DataValue_P6 = DataValue_P6^2-1) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(median = median(DataValue_P6, rm.na = TRUE),
            p10 = quantile(DataValue_P6, probs = 0.10, na.rm=TRUE),
            p25 = quantile(DataValue_P6, probs = 0.25, na.rm=TRUE),
            p90 = quantile(DataValue_P6, probs = 0.9, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(median:p90, names_to = "type", values_to = "change") %>%
  filter(year(date) > 1965, type != "p90") %>%
  ggplot(aes(date,change, color=type)) + geom_line() + theme_classic() +
  geom_hline(yintercept = 0)



#### Bootstrap Loop ####
final_array <- NULL
pce_clear <- pce %>% group_by(date) %>% distinct(LineNumber, .keep_all = TRUE) %>% filter(LineNumber < 340)

for (changes_loop in 3:8){
  for(delay_loop in 0:6){
months_change <- changes_loop
months_delay <- delay_loop
compare_date <- max(pce$date) %m-% months(months_change+months_delay)

recent <-pce_clear %>% group_by(LineDescription) %>%
  mutate(QuantityFinal = Quantity/lag(Quantity,months_change)-1,
         PriceFinal = DataValue/lag(DataValue,months_change)-1,
  ) %>%
  filter(date == max(date) | date == compare_date) %>%
  summarize(QuantityFinal = QuantityFinal[date == max(date)] - QuantityFinal[date == compare_date],
            PriceFinal = PriceFinal[date == max(date)] - PriceFinal[date == compare_date],
            weight = PCEweight[date == max(date)]) %>%
  ungroup()

b <-
  recent %>% inner_join(lowest,by="LineDescription") %>%
  filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  arrange(LineDescription) %>%
  filter(level == "Level 4") %>%
  #left_join(pce_sd, by="LineDescription") %>%
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "Quadrant 1 - Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "Quadrant 2 - Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "Quadrant 3 - Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "Quadrant 4 - Supply+",
    TRUE ~ "Undefined")) %>%
  filter(quadrant != "Undefined") %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW)) %>%
  ungroup() %>%
  arrange(category)


c <- b %>% filter(category == "Services") %>% select(SnW) %>%
  summarize(n = SnW[4]/(SnW[3]+SnW[4]))
c <- as.numeric(c)

final_array <- c(final_array,c)
  }
}

final_array
summary(final_array)


prior_array - final_array


#####

b %>% group_by(category) %>% summarize(sum(nW))
### assume we have shapiro loaded  - cyclical_categories

a <- pce %>% left_join(lowest,by="LineDescription") %>% filter(date == max(date), level=="Level 4") %>% select(LineDescription, LineNumber) %>%
  filter(LineDescription != lag(LineDescription))

View(a)


pce_l <- pce %>% left_join(lowest, by="LineDescription")
pce_l4 <- pce_l %>% filter(level == "Level 4", date == max(date)) %>% distinct(LineNumber, .keep_all = TRUE) %>% group_by(category) %>% summarize(n=n())

sum(pce_l4$n)


b <- pce_l %>% group_by(date,level) %>% summarize(n = n(), w = sum(PCEweight))


b %>%
  ggplot(aes(date,w)) + geom_line() + facet_wrap(~level, scales = "free")