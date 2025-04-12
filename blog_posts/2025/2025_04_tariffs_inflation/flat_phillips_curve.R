# Load necessary libraries
library(scales)
library(lubridate)
library(govMacroTools)
library(tidyverse)

# Obtain core series and process them.
flatpc <- getFRED(c("PCEPILFE", "IA001260M", "unrate"),
                  keep_all = FALSE,
                  rename_variables = c("core_pce", "non_house_services", "unrate")) %>%
  mutate(
    non_house_services = non_house_services / lag(non_house_services, 12) - 1,
    core_pce = core_pce / lag(core_pce, 12) - 1,
    unrate = unrate / 100
  ) %>%
  filter(month(date) %in% c(3, 6, 9, 12))

# Adjust nrou and join
flatpc <- getFRED("nrou") %>%
  mutate(
    nrou = nrou / 100,
    date = date %m+% months(2)
  ) %>%
  inner_join(flatpc, by = "date") %>%
  mutate(ugap = unrate - nrou)

# Reshape the data, compute change_inflation, recode and order factors.
flatpc <- flatpc %>%
  pivot_longer(cols = core_pce:non_house_services,
               names_to = "series",
               values_to = "value") %>%
  group_by(series) %>%
  mutate(change_inflation = value - lag(value, 1)) %>%
  ungroup() %>%
  mutate(
    pre_1991 = if_else(year(date) <= 1990, "1960-1991", "1991-2024"),
    series = recode(series,
                    core_pce = "Core PCE",
                    non_house_services = "Core Non-Housing Services")
  ) %>%
  # Order the facets: put the earlier period on the left and order the series as desired.
  mutate(
    pre_1991 = factor(pre_1991, levels = c("1960-1991", "1991-2024")),
    series = factor(series, levels = c("Core PCE", "Core Non-Housing Services"))
  )


# Define custom colors for the series.
manual_colors <- c("Core PCE" = "#2c3254", 
                   "Core Non-Housing Services" = "#ff8361")

# (Assuming flatpc already has pre_1991 defined as a character variable.)
# Ensure pre_1991 is a factor with the desired order.
flatpc <- flatpc %>%
  mutate(pre_1991 = factor(pre_1991, levels = c("1960-1991", "1991-2024")))

# Create annotation data for the "1960-1991" facet.
# Adjust the x and y values so that the annotation appears where you want it.
annotation_data <- data.frame(
  ugap = 0.015,
  change_inflation = 0.015, 
  pre_1991 = factor("1960-1991", levels = c("1960-1991", "1991-2024"))
)
annotation_data2 <- data.frame(
  ugap = -0.005,              
  change_inflation = 0.02,  
  pre_1991 = factor("1960-1991", levels = c("1960-1991", "1991-2024"))
)

# Create the plot.
ggplot(flatpc, aes(x = ugap, y = change_inflation, color = series)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pre_1991, scales = "free", ncol = 2) +  # Two columns ensure left/right order
  scale_color_manual(values = manual_colors) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  guides(color = "none") +
  theme_classic(base_size = 18) +
  labs(title = "Figure 2: Inflation is Less Driven by Economic Conditions",
       subtitle = "Change in the Rate of Inflation vs. Unemployment Gap",
       y = "Change in Inflation Rate",
       x = "Unemployment Gap",
       caption = "Quarterly change in the year-over-year rate for inflation. Natural unemployment rate from CBO. Mike Konczal.") +
  theme(plot.title.position = "plot") +
  # Add annotation only for the 1960-1991 facet.
  geom_text(
    data = annotation_data, 
    aes(x = ugap, y = change_inflation),
    label = "Core PCE", 
    hjust = 0, 
    size = 6,
    inherit.aes = FALSE,
    color="#2c3254"
  ) +
  geom_text(
    data = annotation_data2, 
    aes(x = ugap, y = change_inflation),
    label = "Core Non-Housing Services PCE", 
    hjust = 0, 
    size = 6,
    inherit.aes = FALSE,
    color="#ff8361"
  ) 