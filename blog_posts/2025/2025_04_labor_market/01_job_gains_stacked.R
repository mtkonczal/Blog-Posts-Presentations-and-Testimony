

# Table 0, Job Gains by Sectors ----
# Get from FRED for easiest  
diff <- getFRED(c("payems", "CES6561000001","CES6562000001", "USGOVT"),
                rename_variables = c("total", "edu", "health", "government"),
                keep_all = FALSE) %>%
  mutate(across(-date, ~.x - lag(.x, 12), .names = "{.col}_diff"),
         year = year(date))

diff_annual <- diff %>% filter(month(date) == 12) %>%
  mutate(total_diff_sub = total_diff - edu_diff - health_diff - government_diff) %>%
  filter(year > 2014)

# Transform data for plotting, remove 2020 data and divide by 12 for monthly values
plot_data <- diff_annual %>%
  filter(year >= 2013) %>%
  mutate(across(c(edu_diff, government_diff, health_diff, total_diff_sub), ~ ifelse(year == 2020, NA, .x / 12))) %>%
  select(year, edu_diff, government_diff, health_diff, total_diff_sub) %>%
  pivot_longer(cols = -year, names_to = "Sector", values_to = "Jobs") %>%
  mutate(Sector = factor(Sector,
                         levels = c("edu_diff", "government_diff", "health_diff", "total_diff_sub"),
                         labels = c("Private Education", "Government", "Health Services", "Rest of Economy")))

# Create stacked bar chart
ggplot(plot_data, aes(x = factor(year), y = Jobs, fill = Sector)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Jobs, 0)),
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  labs(title = "Figure 1: Average Monthly Job Growth by Sector by Year, 2020 Excluded.",
       x = "Year",
       y = "Monthly Job Growth",
       fill = "Sector",
       caption = "End of year annual values. BLS data. Mike Konczal.") +
  scale_fill_manual(values = c(
    "Private Education" = "#8d99ae",
    "Government" = "#2a9d8f",
    "Health Services" = "#6a4c93",
    "Rest of Economy" = "#264653"
  )) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.4, 0.8),
        plot.title.position = "plot")
