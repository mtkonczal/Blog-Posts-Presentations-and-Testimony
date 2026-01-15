
# ------------------------------------------------------------
# 1.  Prep the data  (lines 2, 7, 15, 22)
# ------------------------------------------------------------
big_cut <- 0.01 # label if |contribution| ≥ 0.1 pp
g2_title_components <- "Title"

contr_gdp <- getFRED(c("DPCERY2Q224SBEA", "A007RY2Q224SBEA", "A019RY2Q224SBEA", "A822RY2Q224SBEA"),
  rename_variables = c("Consumption", "Investment", "Net exports", "Government")
) %>%
  mutate(date = date %m+% months(2)) %>%
  pivot_longer(-date, names_to = "series_label", values_to = "value") %>%
  mutate(
    series_label = factor(series_label,
      levels = c(
        "Consumption",
        "Investment",
        "Net exports",
        "Government"
      )
    ),
    value = value/100,
    big_lbl = if_else(abs(value) >= big_cut, # number, no “%”
      number(value * 100, accuracy = 0.1),
      NA_character_
    )
  )

# ------------------------------------------------------------
# 2.  Plot
# ------------------------------------------------------------
plot_components <-
  contr_gdp %>%
  filter(year(date) >= 2023) %>%
  ggplot(aes(date, value, fill = series_label)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  geom_text(aes(label = big_lbl),
    position = position_stack(vjust = 0.5),
    na.rm = TRUE,
    fontface = "bold",
    colour = "white"
  ) +
  scale_fill_viridis_d(option = "D", name = NULL) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0, .05))
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    subtitle = "Components of real GDP growth by contribution, quarterly change annualised.",
    caption = "Source: BEA NIPA Table 1.1.2",
    x = NULL, y = NULL
  ) +
  facet_wrap(~series_label) +
  theme_chartbook()
