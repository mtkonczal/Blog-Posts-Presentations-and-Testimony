

housing <- getFRED(c("HOUST", "HOUST1F")) %>%
  pivot_longer(-date, names_to = "type", values_to = "value") %>%
  mutate(type = case_when(
    type == "houst" ~ "Total Units",
    type == "houst1f" ~ "Single-Family Units",
    TRUE ~ "ERROR"
  ))


housing_graphic <- housing %>%
  filter(year(date) >= 2018) %>%
  group_by(type) %>%
  mutate(last_value = value[date == max(date)]) %>%
  ggplot(aes(date, value, color = type)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(~type) +
  labs(
    subtitle = "New Privately-Owned Housing Units Started",
    x = "",
    y = ""
  ) +
  facet_wrap(~type) +
  geom_line(aes(date, last_value, color = type), linetype = "dashed", show.legend = FALSE) +
  theme_chartbook() +
  scale_chartbook_colors()