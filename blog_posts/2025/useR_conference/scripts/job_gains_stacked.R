
# Table 0, Job Gains by Sectors ----
# Get from FRED for easiest
diff <- getFRED(c("payems", "CES6561000001", "CES6562000001", "USGOVT"),
  rename_variables = c("total", "edu", "health", "government"),
  keep_all = FALSE
) %>%
  mutate(across(-date, ~ .x - lag(.x, 1), .names = "{.col}_diff")) %>%
  reframe(
    date = date,
    gov_edu_health = edu_diff + health_diff + government_diff,
    rest_diff = total_diff - gov_edu_health
  ) %>%
  filter(year(date) > 2022)

# Transform data for plotting, remove 2020 data and divide by 12 for monthly values
plot_data <- diff %>%
  pivot_longer(cols = -date, names_to = "Sector", values_to = "Jobs") %>%
  mutate(Sector = factor(Sector,
                         levels = c("gov_edu_health", "rest_diff", "health_diff", "total_diff_sub"),
                         labels = c("Government, Private Health and Education", "Rest of Jobs", "Health Services", "Rest of Economy")))

# Create stacked bar chart
plot_data_cyc_graphic <- ggplot(plot_data, aes(x = date, y = Jobs, fill = Sector)) +
  geom_bar(stat = "identity") +
  labs(subtitle = "Monthly Job Growth in Cyclical versus Noncyclical Industries.",
       ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 14) +
  theme_chartbook() +
  scale_chartbook_fill() +
  theme(legend.position = "top")
