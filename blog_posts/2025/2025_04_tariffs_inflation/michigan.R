library(tidyverse)
library(govMacroTools)
library(scales)
library(lubridate)


mi <- read_csv("data/michigan_5_years.csv")


# Step 1: Create a proper Date column
df <- mi %>%
  mutate(
    month_num = match(Month, month.name),
    date = ymd(paste(YYYY, month_num, "01", sep = "-"))
  )

library(scales)  # for percent_format

df <- df %>%
  filter(year(date) > 1990) %>%
  mutate(PX5_MD = PX5_MD / 100,
         point_df = if_else(date == max(date), PX5_MD, NA_real_))

df %>%
  ggplot(aes(x = date, y = PX5_MD)) +
  geom_line(na.rm = TRUE, color = "steelblue", size = 1.2) +
  geom_point(aes(y = point_df), size = 3, color = "steelblue", na.rm = TRUE) +
  geom_text(
    data = . %>% filter(date == max(date)),
    aes(label = scales::percent(PX5_MD, accuracy = 0.1)),
    vjust = -1,
    fontface = "bold",
    color = "steelblue"
  ) +
  labs(
    title = "Figure 3: Expected Inflation Significantly Higher Than Post-COVID Inflation",
    subtitle = "Michigan Survey of Consumers: Expected Change in Prices Over Next 5-10 Years",
    x = "",
    y = "",
    caption = "University of Michigan, Mike Konczal"
  ) +
  theme_classic(base_size = 14) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "24 months") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title.position = "plot"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  geom_point(data = df %>% filter(year(date) == 2025),
                           aes(date, PX5_MD))




df