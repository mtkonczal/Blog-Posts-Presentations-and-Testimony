

us_dollar <- getFRED(c("TWEXBGSMTH")) %>%
  rename(us_dollar = twexbgsmth)

t10 <- getFRED(c("dgs10")) %>%
  rename(treasury_10 = dgs10)



  mutate(type = case_when(
    type == "twexbgsmth" ~ "U.S. Dollar Index",
    type == "dgs10" ~ "10-Year U.S. Treasury",
    TRUE ~ "ERROR"
  ))




"DGS10"
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
  theme_classic(base_size = 18) +
  facet_wrap(~type) +
  geom_line(aes(date, last_value, color = type), linetype = "dashed", show.legend = FALSE) +
  theme(plot.title.position = "plot")