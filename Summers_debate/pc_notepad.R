# Install and load required packages
library(quantmod)
library(tidyverse)
library(lubridate)
library(latex2exp)
library(readxl)
library(hrbrthemes)
library(scales)


##### SET UP SOME THINGS #####
theme_lass <-   theme_modern_rc(ticks = TRUE) + theme(legend.position = "none", legend.title = element_blank(),
                                                      panel.grid.major.y = element_line(size=0.5),
                                                      panel.grid.minor.y = element_blank(),
                                                      plot.title.position = "plot",
                                                      axis.title.x = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      plot.title = element_text(size = 25, face="bold"),
                                                      plot.subtitle = element_text(size=15, color="white"),
                                                      plot.caption = element_text(size=10, face="italic"),
                                                      legend.text = element_text(size=12),
                                                      axis.text.y = element_text(size=12, face="bold"),
                                                      axis.text.x = element_text(size=12, face="bold"),
                                                      strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                                      panel.grid.major.x = element_blank(),
                                                      panel.grid.minor.x = element_blank(),
                                                      strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))


# Download data
getSymbols("EXPINF5YR", src="FRED")  
getSymbols("UNRATE", src="FRED")      
getSymbols("NROU", src="FRED")       
getSymbols("PCEPILFE", src="FRED")
getSymbols("MICH", src="FRED")

# Convert xts objects to data frames
expinf_df <- data.frame(Date = index(EXPINF5YR), Expected_Inflation = as.numeric(EXPINF5YR$EXPINF5YR))
unrate_df <- data.frame(Date = index(UNRATE), Unemployment = as.numeric(UNRATE$UNRATE))
nrou_df <- data.frame(Date = index(NROU), Noncyclical_Unemployment = as.numeric(NROU$NROU))
pce_df <- data.frame(Date = index(PCEPILFE), PCE_Core_Inflation = as.numeric(PCEPILFE$PCEPILFE))
rm(UNRATE, NROU, PCEPILFE, EXPINF5YR)


# Import variable
getSymbols("IREXPETCOM", src="FRED")
import_inflation <- data.frame(Date = index(IREXPETCOM), import_inflation = as.numeric(IREXPETCOM$IREXPETCOM))
getSymbols("A255RC1Q027SBEA", src="FRED")
import_nominal <- data.frame(Date = index(A255RC1Q027SBEA), import_nominal = as.numeric(A255RC1Q027SBEA$A255RC1Q027SBEA)) 
getSymbols("GDP", src="FRED")
gdp <- data.frame(Date = index(GDP), gdp = as.numeric(GDP$GDP))
rm(GDP, IREXPETCOM, A255RC1Q027SBEA)


supply_chain <-read_csv("data/supply_chain.csv")
supply_chain$date <- as.Date(paste(supply_chain$year, supply_chain$month,1, sep = "-"), "%Y-%m-%d")
supply_chain <- supply_chain %>% select(-month, -year) %>% rename(Date = date)


import_inflation <-
  import_inflation %>%
  left_join(gdp, by="Date") %>%
  left_join(import_nominal, by="Date") %>%
  left_join(pce_df, by="Date") %>%
  left_join(supply_chain, by="Date") %>%
  na.omit() %>%
  mutate(import_gdp = import_nominal/gdp,
         import_inflation = (import_inflation/lag(import_inflation,1))^4-1,
         PCE_Core_Inflation = PCE_Core_Inflation/lag(PCE_Core_Inflation,4)-1) %>%
  na.omit() %>%
  mutate(import_variable = import_inflation - PCE_Core_Inflation,
         import_variable = import_variable*import_gdp) %>%
    select(Date, import_variable)
  

#### Version 1 without Imports ####
# Merge data and remove rows with NA values
df <- expinf_df %>%
  full_join(unrate_df, by = "Date") %>%
  full_join(nrou_df, by = "Date") %>%
  full_join(pce_df, by = "Date") %>%
  na.omit()

# Create the new variables
df <- df %>%
  mutate(
    PCE_Change_Annualized = (PCE_Core_Inflation/lag(PCE_Core_Inflation,1))^4 - 1,
    PCE_Change_Annualized = PCE_Change_Annualized*100,
    Unrate_Nrou_Diff = Unemployment - Noncyclical_Unemployment
  ) %>%
  na.omit()

head(df)


# Subset data for 1980-2019
df_1980_2019 <- df %>% filter(Date >= as.Date("1980-01-01") & Date <= as.Date("2019-12-31"))

# Run regression: PCE_Core_Inflation ~ lagged PCE_Core_Inflation + Expected_Inflation + Noncyclical_Unemployment
model <- lm(PCE_Change_Annualized ~ lag(PCE_Change_Annualized, 1) + lag(PCE_Change_Annualized, 2) + Expected_Inflation + Unrate_Nrou_Diff, data = df_1980_2019)

summary(model)


df$Predicted_Inflation <- predict(model, newdata = df)

ggplot(df, aes(x = Date)) +
  geom_line(aes(y = PCE_Change_Annualized, color = "Actual Inflation")) +
  geom_line(aes(y = Predicted_Inflation, color = "Predicted Inflation")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation", y = "Inflation Rate", x = "Date") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation" = "red")) +
  theme_minimal()




#### Version 2 - Monthly ####
# Merge data and remove rows with NA values
df <- expinf_df %>%
  full_join(unrate_df, by = "Date") %>%
  left_join(nrou_df, by = "Date") %>%
  full_join(pce_df, by = "Date") #%>%
  #left_join(supply_chain, by="Date")

for (i in 1:nrow(df))
  if(is.na(df$Noncyclical_Unemployment[i]))
     df$Noncyclical_Unemployment[i] = df$Noncyclical_Unemployment[i-1]

# Create the new variables
df <- df %>%
  mutate(
    PCE_Change_Annualized = (PCE_Core_Inflation/lag(PCE_Core_Inflation,3))^4 - 1,
    PCE_Change_Annualized = PCE_Change_Annualized*100,
    Unrate_Nrou_Diff = Unemployment - Noncyclical_Unemployment
  ) %>%
  na.omit() %>%
  filter(month(Date) %in% c(6,3,12,9))

head(df)
tail(df)



# Subset data for 1980-2019
df_1980_2019 <- df %>% filter(Date >= as.Date("1980-01-01") & Date <= as.Date("2019-12-31"))

# Run regression: PCE_Core_Inflation ~ lagged PCE_Core_Inflation + Expected_Inflation + Noncyclical_Unemployment
model <- lm(PCE_Change_Annualized ~ lag(PCE_Change_Annualized, 1) + lag(PCE_Change_Annualized, 2) + Expected_Inflation + Unrate_Nrou_Diff, data = df_1980_2019)

summary(model)


df$Predicted_Inflation <- predict(model, newdata = df)

ggplot(df, aes(x = Date)) +
  geom_line(aes(y = PCE_Change_Annualized, color = "Actual Inflation")) +
  geom_line(aes(y = Predicted_Inflation, color = "Predicted Inflation")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is quarterly 1982-2019, quarters starting in July, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Cleveland Fed 5-year expected inflation used for expectations. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation" = "red")) +
  theme_minimal() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot")



##### Long veresion #####

long_exp <- read_delim("data/LONGBASE.TXT") %>%
  select(OBS, PTR) %>%
  mutate(year = as.numeric(substr(OBS, 1, 4)), quarter = as.numeric(substr(OBS, 6, 6))) %>%
  mutate(month = quarter*2+quarter-2) %>%
  mutate(Date = as.Date(paste(year, month,1, sep = "-"), "%Y-%m-%d")) %>%
  select(Date, FRB_exp = PTR)


df <- unrate_df %>%
  full_join(long_exp, by = "Date") %>%
  left_join(nrou_df, by = "Date") %>%
  full_join(pce_df, by = "Date") %>%
  filter(year(Date)>= 1962)

df <- as_tibble(df)

for (i in 1:nrow(df))
  if(is.na(df$Noncyclical_Unemployment[i])){
    df$Noncyclical_Unemployment[i] = df$Noncyclical_Unemployment[i-1]
    df$FRB_exp[i] = df$FRB_exp[i-1]}

# Create the new variables
df <- df %>%
  mutate(
    PCE_Change_Annualized = (PCE_Core_Inflation/lag(PCE_Core_Inflation,3))^4 - 1,
    PCE_Change_Annualized = PCE_Change_Annualized*100,
    Unrate_Nrou_Diff = Unemployment - Noncyclical_Unemployment
  ) %>%
  na.omit() %>%
  filter(month(Date) %in% c(6,3,12,9))

head(df)
tail(df)



# Subset data for 1980-2019
df_1962_2019 <- df %>% filter(Date <= as.Date("2019-12-31")) %>% filter(year(Date)>1981)

# Run regression: PCE_Core_Inflation ~ lagged PCE_Core_Inflation + Expected_Inflation + Noncyclical_Unemployment
model <- lm(PCE_Change_Annualized ~ lag(PCE_Change_Annualized, 1) + lag(PCE_Change_Annualized, 2) + FRB_exp + Unrate_Nrou_Diff, data = df_1962_2019)

summary(model)


df$Predicted_Inflation <- predict(model, newdata = df)

ggplot(df, aes(x = Date)) +
  geom_line(aes(y = PCE_Change_Annualized, color = "Actual Inflation")) +
  geom_line(aes(y = Predicted_Inflation, color = "Predicted Inflation")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is quarterly 1982-2019, quarters starting in July, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Cleveland Fed 5-year expected inflation used for expectations. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation" = "red")) +
  theme_minimal() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot")


#### Jolts ####

getSymbols("JTSJOR", src="FRED")
jo <- as_tibble(data.frame(Date = index(JTSJOR), openings = as.numeric(JTSJOR$JTSJOR)))

long_jo <- read_csv("data/historical jolts.csv") %>% select(Date = date, job_opening_rate_long = job_opening_rate)

long_jo <- long_jo %>% full_join(jo, by="Date") %>%
  mutate(actual_opening = if_else(!is.na(openings),openings, job_opening_rate_long)) %>%
  select(Date, long_openings = actual_opening)

df <- unrate_df %>%
  full_join(long_exp, by = "Date") %>%
  left_join(nrou_df, by = "Date") %>%
  full_join(pce_df, by = "Date") %>%
  left_join(long_jo, by="Date") %>%
  filter(year(Date)>= 1962)

df <- as_tibble(df)

for (i in 1:nrow(df))
  if(is.na(df$Noncyclical_Unemployment[i])){
    df$Noncyclical_Unemployment[i] = df$Noncyclical_Unemployment[i-1]
    df$FRB_exp[i] = df$FRB_exp[i-1]}

# Create the new variables
df <- df %>%
  mutate(
    PCE_Change_Annualized = (PCE_Core_Inflation/lag(PCE_Core_Inflation,3))^4 - 1,
    PCE_Change_Annualized = PCE_Change_Annualized*100,
    Unrate_Nrou_Diff = Unemployment - Noncyclical_Unemployment,
    u_v_ratio = long_openings/Unemployment,
    u_v_ratio_star = long_openings/Unemployment - long_openings/Noncyclical_Unemployment
  ) %>%
  na.omit() %>%
  filter(month(Date) %in% c(6,3,12,9))

head(df)
tail(df)



# Subset data for 1980-2019
df_1962_2019 <- df %>% filter(Date <= as.Date("2019-12-31")) %>% filter(year(Date)>1961)

# Run regression: PCE_Core_Inflation ~ lagged PCE_Core_Inflation + Expected_Inflation + Noncyclical_Unemployment
model <- lm(PCE_Change_Annualized ~ lag(PCE_Change_Annualized, 1) + lag(PCE_Change_Annualized, 2) + FRB_exp + u_v_ratio_star, data = df_1962_2019)

summary(model)


df$Predicted_Inflation <- predict(model, newdata = df)

df %>% filter(year(Date)>2014) %>%
ggplot(aes(x = Date)) +
  geom_line(aes(y = PCE_Change_Annualized, color = "Actual Inflation")) +
  geom_line(aes(y = Predicted_Inflation, color = "Predicted Inflation")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is quarterly 1982-2019, quarters starting in July, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Cleveland Fed 5-year expected inflation used for expectations. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation" = "red")) +
  theme_minimal() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot")




long_exp <- read_delim("data/HISTDATA.TXT") %>%
  select(OBS, PTR) %>%
  mutate(year = as.numeric(substr(OBS, 1, 4)), quarter = as.numeric(substr(OBS, 6, 6))) %>%
  mutate(month = quarter*2+quarter-2) %>%
  mutate(date = as.Date(paste(year, month,1, sep = "-"), "%Y-%m-%d")) %>%
  select(date, FRB_exp = PTR) %>%
  mutate(FRB_exp = FRB_exp/100)

exp_breaks <- long_exp %>% filter(month(date)==1)
exp_breaks <- exp_breaks$date
exp_breaks <- exp_breaks[seq(1, length(exp_breaks), 3)]

long_exp %>%
  ggplot(aes(date, FRB_exp)) + geom_line() + theme_lass +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = exp_breaks) +
  labs(title="Not Expecting Much",
       subtitle="10-year inflation expectations from FRB/US model's historical data (see caption below).",
       caption="1991- = Survey of Professional Forecasters (SPF).
1981 to 1991 = Hoey Survey.
1968-1981 = Constructed via Kozicki and Tinsley (2001)
Mike Konczal, Roosevelt Institute")