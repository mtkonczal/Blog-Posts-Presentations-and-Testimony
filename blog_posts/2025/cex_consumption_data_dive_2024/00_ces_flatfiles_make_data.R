library(tidyverse)
library(janitor)
library(tidyusmacro)
library(tibble)


df_items <- tibble::tibble(
  item = c(
    "Number of consumer units (in thousands)(a)",
    "Average income before taxes",
    "Average annual expenditures",
    "Food",
    "Food at home",
    "Cereals and bakery products",
    "Meats, poultry, fish, and eggs",
    "Dairy products",
    "Fruits and vegetables",
    "Other food at home",
    "Food away from home",
    "Alcoholic beverages",
    "Housing",
    "Owned dwellings",
    "Rented dwellings",
    "Other lodging",
    "Apparel and services",
    "Transportation",
    "Vehicle purchases (net outlay)",
    "Gasoline",
    "Other vehicle expenses",
    "Vehicle insurance",
    "Public and other transportation",
    "Healthcare",
    "Health insurance",
    "Medical services",
    "Drugs",
    "Medical supplies",
    "Entertainment",
    "Personal care products and services",
    "Reading",
    "Education",
    "Tobacco products and smoking supplies",
    "Miscellaneous",
    "Cash contributions",
    "Personal insurance and pensions",
    "Life and other personal insurance",
    "Retirement, pensions, and Social Security",
    "Contributions to retirement plans",
    "Deductions for Social Security"
  ),
  indent = c(
    0,
    0,
    0,
    0,
    1,
    2,
    2,
    2,
    2,
    2,
    1,
    0,
    0,
    1,
    1,
    1,
    0,
    0,
    1,
    2,
    1,
    2,
    1,
    0,
    1,
    1,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    2,
    2
  )
)

df_items %>% filter(indent == 0)

demographics_characteristics <- tibble(
  demographics_code = c("LB01", "LB01", "LB01", "LB01", "LB01", "LB01", "LB04"),
  characteristics_code = c("01", "02", "03", "04", "05", "06", "03"),
  filtered_keep = 1
)


cex_org <- getBLSFiles("cex", "konczal@gmail.com")

cex <- cex_org %>% select(!ends_with(".x")) %>% select(!ends_with(".y"))

total_expend_cat <- c(
  "CXUTOTALEXPLB0101M",
  "CXUTOTALEXPLB0102M",
  "CXUTOTALEXPLB0103M",
  "CXUTOTALEXPLB0104M",
  "CXUTOTALEXPLB0105M",
  "CXUTOTALEXPLB0106M"
)

total_expend <- cex %>%
  filter(series_id %in% total_expend_cat) %>%
  select(
    year,
    characteristics_code,
    demographics_code,
    total_expend = value
  )

percent_expenditure <- cex %>%
  filter(
    category_text == "Expenditures",
    item_text %in% df_items$item
  ) %>%
  # Note we're doing this here!
  left_join(
    demographics_characteristics,
    by = c("characteristics_code", "demographics_code")
  ) %>%
  filter(filtered_keep == 1) %>%
  left_join(
    total_expend,
    by = c("characteristics_code", "demographics_code", "year")
  ) %>%
  mutate(percent_expenditure = value / total_expend)


write_csv(percent_expenditure, "cex_data.csv")
