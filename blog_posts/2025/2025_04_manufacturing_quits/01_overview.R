



jolts %>% 
  filter(
    display_level.x == 0,
    seasonal == "S",
    dataelement_code %in% c("QU","JO","HI"),
    ratelevel_code   == "R",
    state_text       == "Total US"
  ) %>%
  select(dataelement_text, date, value) %>%
  group_by(dataelement_text) %>%
  mutate(date = date, value = value/100,
         value_6m = slide_dbl(value, mean, .before = 5, .complete = TRUE)) %>%
  ggplot(aes(date, value_6m)) +
  geom_line(color="#2c3254") +
  theme_classic(base_size = 15) +
  scale_y_continuous(label = percent) +
  facet_wrap(~dataelement_text, scales = "free") +
  labs(title="Figure 1: 2021-2023 Saw Record Quitting, Hiring, and Openings",
       caption = "Mike Konczal",
       subtitle="Six-month average rate. Total nonfarm.",
       y="",
       x="") +
  theme(plot.title.position = "plot")

ggsave("graphics/g1_overview.png", width = 2190, height = 1200, dpi = 300, units = "px")



highlight_clr <- "#2c3254"   # same dark blue you used before
other_clr     <- "grey80"

jolts %>% 
  filter(
    display_level.x == 2,
    period == "M13",
    dataelement_code == "QU",
    year == 2017,
    ratelevel_code   == "R",
    state_text       == "Total US"
  ) %>% 
  mutate(
    value         = value / 100,                                    # rate → proportion
    industry_text = fct_reorder(industry_text, value) # hi → low order
  ) %>% 
  ggplot(aes(industry_text, value,
             fill = industry_text == "Manufacturing")) +            # TRUE / FALSE flag
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = highlight_clr,
                               `FALSE` = other_clr),
                    guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_classic(base_size = 15) +
  labs(
    title    = "Quit Rates by Industry, 2017 (Annual-Average)",
    subtitle = "Display-level 2 industries, seasonally adjusted ‒ Manufacturing highlighted",
    caption  = "Source: BLS JOLTS. Mike Konczal",
    x = NULL, y = NULL
  ) +
  theme(plot.title.position = "plot")

ggsave("graphics/g0_overview.png", width = 2190, height = 1000, dpi = 300, units = "px")