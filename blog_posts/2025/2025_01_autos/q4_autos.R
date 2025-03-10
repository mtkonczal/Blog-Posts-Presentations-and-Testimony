source("getFRED.R")
library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)
library(viridis)
library(bea.R)
library(forcats)


# Generate 100 dates, starting from the maximum date in the input vector and going back X months at a time
generate_dates <- function(dates, X) {
  max_date <- max(dates)
  generated_dates <- seq(max_date, length.out = 100, by = paste0("-", X, " months"))
  return(generated_dates)
}


get_NIPA_data <- function(beaKey, TableName, Frequency, Year, data_set_name = 'NIPA'){
  NIPA_request <- list(
    'UserID' = beaKey ,
    'Method' = 'GetData',
    'datasetname' = data_set_name,
    'TableName' = TableName,
    'Frequency' = Frequency,
    'Year' = Year,
    'ResultFormat' = 'json'
  );
  NIPA_data <- beaGet(NIPA_request, asWide = FALSE)
  return(NIPA_data)
}

BEA_date_quarterly <- function(x){
  x <- x %>%
    mutate(year = substr(TimePeriod, 1, 4)) %>%
    mutate(quarter = substr(TimePeriod, 5,6)) %>%
    mutate(month = case_when(
      quarter == "Q1" ~ 3,
      quarter == "Q2" ~ 6,
      quarter == "Q3" ~ 9,
      quarter == "Q4" ~ 12))
  x$date <- paste(x$month, "01", x$year, sep="/")
  x$date <- as.Date(x$date, "%m/%d/%Y")
  x <- x %>% select(-month, -quarter, -year)
  return(x)
}



auto_sales <- as_tibble(getFRED(c("TOTALSA"),
                        rename_variables = c("total_autos"),
                        keep_all = FALSE))

# How many extra cars?
sum(tail(auto_sales$total_autos,2))/12 - 2*((sum(tail(auto_sales$total_autos,24)) - sum(tail(auto_sales$total_autos,2)))/22)/12
#[1] 0.1884773


#First Graphic - Total Auto Sales
auto_sales %>%
  filter(year(date) >= 2018) %>%
  ggplot(aes(x = date, y = total_autos)) +
  geom_line(color = "#005A9C", size = 1.2) +  # Using blue from Set1
  labs(
    title    = "An Extra 188,500 Total Cars Sold Anticipating Trump's Tariffs?",
    subtitle = "Total Vehicle Sales, Millions of Units, Monthly, Seasonally Adjusted Annual Rate.",
    x        = "",
    y        = "",
    caption  = "BEA, Supplemental Motor vehicles. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(auto_sales$date, 12),
    date_labels = "%b %Y"
  ) +
  theme_classic(base_size = 9) +
  theme(
    plot.title          = element_text(face = "bold", size = 17),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title.position = "plot"
  )
ggsave("tester1.png", width = 8, height = 4, units = "in", dpi = "retina")


#


rgdp <- as_tibble(getFRED(c("A191RL1Q225SBEA","DPCERY2Q224SBEA","DDURRY2Q224SBEA"),
                                rename_variables = c("real_gdp","cont_pce","cont_durable_goods")) %>%
            mutate(date = date %m+% months(2),
                   cont_pce_excl_durable = cont_pce - cont_durable_goods,
                   everything_else = real_gdp - cont_pce))

rgdp %>%
  filter(year(date) >= 2022) %>%
  select(-real_gdp, -cont_pce) %>%
  pivot_longer(cont_durable_goods:everything_else, names_to = "type", values_to = "values") %>%
  mutate(type = case_when(
    type == "cont_durable_goods"    ~ "Durable Goods",
    type == "cont_pce_excl_durable"   ~ "PCE Ex Durables",
    type == "everything_else"         ~ "Other GDP",
    TRUE                            ~ type  # fallback in case of unexpected values
  )) %>%
  ggplot(aes(date, values, fill = type, label = values)) + geom_bar(stat = 'identity', size=0) +
  scale_fill_brewer(palette="Set1") +
  theme_classic(base_size = 9) +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  theme(legend.title = element_blank(), legend.text = element_text(size=8)) +
  labs(
    title    = "Durable Goods Supported GDP in Q4 2024",
    subtitle = "Contribution to Real GDP.",
    x        = "",
    y        = "",
    caption  = "BEA, 1.1.2. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(auto_sales$date, 12),
    date_labels = "%b %Y"
  ) +
  theme(
    plot.title          = element_text(face = "bold", size = 17),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title.position = "plot"
  )

ggsave("tester2.png", width = 8, height = 4, units = "in", dpi = "retina")


#Graphic 3


beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

beaR_dates <- '2023,2024'
lines_nos <- c(5:8, 10:13, 16:22)

ngdp <- get_NIPA_data(beaKey, 'T10505', 'Q', beaR_dates, data_set_name = 'NIPA')
ngdp <- BEA_date_quarterly(ngdp)

ngdp_df <- ngdp %>% group_by(LineNumber) %>%
  mutate(change = (DataValue/lag(DataValue,1))^4-1,
         previous_change = lag(DataValue,1)/lag(DataValue,5)-1,
         increased_rate = change - previous_change) %>%
  ungroup() %>%
  filter(date == max(date),
         LineNumber %in% lines_nos) %>%
  mutate(
    type = case_when(
      LineNumber %in% 5:8   ~ "Durable Goods",
      LineNumber %in% 10:13 ~ "Nondurable Goods",
      LineNumber %in% 16:22 ~ "Services",
      TRUE                  ~ "other"
    )
  )
  
ngdp_df %>%
  mutate(LineDescription2 = fct_reorder(LineDescription, increased_rate)) %>%
  ggplot(aes(x = increased_rate, y = LineDescription2, fill = type)) +
  # Draw the bars
  geom_col() +
  # Add a label in percent, just outside each bar
  geom_text(
    aes(
      label = percent(increased_rate, accuracy = 1),
      # If the value is > 0, push label to the right, otherwise to the left
      hjust = if_else(increased_rate > 0, -0.1, 1.1)
    ),
    color = "black",
    size = 3
  ) +
  # Format the x-axis as percentages
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 9) +
  theme(
    legend.title          = element_blank(),
    legend.text           = element_text(size = 8),
    plot.title            = element_text(face = "bold", size = 17),
    plot.caption          = element_text(size = 12),
    axis.title            = element_text(face = "bold"),
    axis.text.x           = element_text(angle = 45, hjust = 1),
    plot.title.position   = "plot"
  ) +
  # Final labels (these override any previous labs(x, y, etc.))
  labs(
    title    = "Nominal Spending Accelerated for Durable Goods, Especially Autos",
    subtitle = "Difference between Q4:2024 nominal spending (annualized) and Q3:2024 year over previous year.",
    x        = "",
    y        = "",
    fill     = "Type",
    caption  = "BEA, 1.5.5. Mike Konczal"
  ) +
  # Allows labels to be placed outside the plot region
  coord_cartesian(clip = "off")

ggsave("tester3.png", width = 8, height = 4, units = "in", dpi = "retina")




#### Last Graphic ####
auto_invt <- as_tibble(getFRED(c("AISRSA"),
                                rename_variables = c("auto_inventory"),
                                keep_all = FALSE))


#First Graphic - Total Auto Sales
auto_invt %>%
  filter(year(date) >= 2018) %>%
  ggplot(aes(x = date, y = auto_inventory)) +
  geom_line(color = "#005A9C", size = 1.2) +  # Using blue from Set1
  labs(
    title    = "Auto Inventories Haven't Recovered During the Pandemic",
    subtitle = "Auto Inventory/Sales Ratio.",
    x        = "",
    y        = "",
    caption  = "BEA, Supplemental Motor vehicles. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(auto_sales$date, 12),
    date_labels = "%b %Y"
  ) +
  theme_classic(base_size = 9) +
  theme(
    plot.title          = element_text(face = "bold", size = 17),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title.position = "plot"
  )
ggsave("tester4.png", width = 8, height = 4, units = "in", dpi = "retina")

