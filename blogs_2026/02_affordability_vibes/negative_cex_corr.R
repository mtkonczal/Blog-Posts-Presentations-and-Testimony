library(tidyverse)
library(janitor)
library(tidyusmacro)
library(broom)
library(purrr)


df_items <- tibble::tibble(
  item = c(
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

demographics_characteristics <- bind_rows(
  tibble(
    demographics_code = "LB01",
    characteristics_code = c("01", "02", "03", "04", "05", "06"),
    filtered_keep = 1
  ),
  tibble(
    demographics_code = "LB04",
    characteristics_code = c("01", "02", "03", "04", "05", "06", "07", "08", "09"),
    filtered_keep = 1
  )
)


cex_org <- getBLSFiles("cex", "konczal@gmail.com")

cex <- cex_org %>% select(!ends_with(".x")) %>% select(!ends_with(".y"))

total_expend_cat <- cex %>%
  filter(item_text == "Average annual expenditures") %>%
  semi_join(
    demographics_characteristics,
    by = c("characteristics_code", "demographics_code")
  ) %>%
  distinct(series_id) %>%
  pull(series_id)

total_expend <- cex %>%
  filter(series_id %in% total_expend_cat) %>%
  select(
    year,
    characteristics_code,
    demographics_code,
    total_expend = value
  )

percent_expenditure <- cex %>%
  filter(item_text %in% df_items$item) %>%
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
  mutate(percent_expenditure = if_else(
    category_text == "Expenditures",
    value / total_expend,
    NA_real_
  ))





reg_results <- percent_expenditure %>%
  filter(demographics_text == "Quintiles of income before taxes",
         characteristics_text != "All Consumer Units") %>%
  select(year, item_text, percent_expenditure, total_expend) %>%
  group_by(item_text) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(percent_expenditure ~ total_expend, data = .x)),
    tidy  = map(model, tidy),
    glance = map(model, glance)
  )


summary_table <- reg_results %>%
  mutate(
    slope = map_dbl(model, ~ coef(.x)["total_expend"]),
    r2    = map_dbl(model, ~ glance(.x)$r.squared),
    n     = map_dbl(model, ~ glance(.x)$nobs)
  ) %>%
  select(item_text, slope, r2, n) %>%
  arrange(slope)

summary_table

cpi <- getFRED(cpi = "CPIAUCSL")

cpi <- cpi %>% mutate(year = year(date)) %>%
  group_by(year) %>%
  reframe(cpi = mean(cpi))

percent_expenditure %>%
  filter(demographics_text == "Quintiles of income before taxes",
         characteristics_text != "All Consumer Units",
         item_text %in% c("Rented dwellings", "Owned dwellings")) %>%
  select(year, item_text, percent_expenditure, total_expend) %>%
  left_join(cpi, by="year") %>%
  mutate(real_expend = total_expend/cpi) %>%
  ggplot(aes(total_expend, percent_expenditure, color=item_text)) + geom_point()