

# Filter and select required data
ces %>% 
  filter(data_type_text == "DIFFUSION INDEXES, 12-MONTH SPAN, NOT SEASONALLY ADJUSTED",
         industry_code == "05000000") %>%
  select(date, value) %>%
  mutate(value = value/100) %>%
  filter(year(date) <= 2024) %>%
  mutate(tag = if_else(date == max(date), value, NA)) %>%
ggplot(aes(x = date, y = value, label = tag)) +
  geom_line(size = 1.2, color="#264653") +
  labs(
    title = "Figure 4: Though Lower, Majority of Sub-Industries Have Positive Job Growth",
    subtitle = "12-Month span total private diffusion index, through December 2024.",
    x = "",
    y = "",
    caption = "BLS, Mike Konczal."
  ) +
  theme_classic(base_size = 14) +
  geom_hline(yintercept = 0.5) +
  theme(plot.title.position = "plot") +
  geom_text(aes(label = scales::percent(tag, accuracy = 1)), nudge_x = 400, na.rm = TRUE) +
  geom_point(aes(date, tag), color="#264653", size=2) +
  scale_y_continuous(label = percent)