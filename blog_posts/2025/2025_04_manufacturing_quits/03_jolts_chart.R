

## ----------------- 1.  User settings ------------------------------------
base_year   <- 2019
comp_year   <- 2022
use_relative <- TRUE      # TRUE -> % change   |   FALSE -> absolute change

## ----------------- 2.  Pretty industry names ----------------------------
nice_names <- c(
  "Construction"                          = "Construction",
  "Federal"                               = "Federal",
  "Financial activities"                  = "Financial",
  "Information"                           = "Information",
  "Leisure and hospitality"               = "Leisure & Hosp.",
  "Manufacturing"                         = "Manufacturing",
  "Mining and logging"                    = "Mining & Log.",
  "Other services"                        = "Other services",
  "Private education and health services" = "Private Ed. & Health",
  "Professional and business services"    = "Prof. & Bus. Svcs.",
  "State and local"                       = "State & Local",
  "Trade, transportation, and utilities"  = "Trade & Transp."
)
 ## Adjust

flows <- jolts %>% 
  filter(
    period          == "M13",
    display_level.x == 2,                                 # two‑digit industries
    dataelement_code %in% c("HI", "JO", "QU"),            # Hires, Openings, Quits
    ratelevel_code   == "R",
    state_text       == "Total US",
    year %in% c(base_year, comp_year)
  ) %>% 
  select(industry_text, dataelement_code, year, value) %>% 
  mutate(industry_text = recode(industry_text, !!!nice_names))

## 3a. Main flows: percent & absolute -------------------------------------
change_flows <- flows %>% 
  group_by(industry_text, dataelement_code) %>% 
  summarise(
    v_base = value[year == base_year],
    v_comp = value[year == comp_year],
    .groups = "drop"
  ) %>% 
  mutate(
    pct_change  = (v_comp / v_base) - 1,
    abs_change  = v_comp - v_base,
    measure = recode(
      dataelement_code,
      "HI" = "Hires",
      "JO" = "Job Openings",
      "QU" = "Quits"
    )
  )

## 3b. Openings / Hires ratio ---------------------------------------------
ratio <- flows %>% 
  pivot_wider(names_from = dataelement_code, values_from = value) %>% 
  group_by(industry_text) %>% 
  summarise(
    ratio_base = (JO/HI)[year == base_year],
    ratio_comp = (JO/HI)[year == comp_year],
    .groups = "drop"
  ) %>% 
  mutate(
    pct_change = (ratio_comp / ratio_base) - 1,
    abs_change = ratio_comp - ratio_base,
    measure    = "Openings / Hires"
  )

## ----------------- 4.  Combine, pick metric, order within facet ----------
plot_df <- bind_rows(
  change_flows %>% select(industry_text, pct_change, abs_change, measure),
  ratio        %>% select(industry_text, pct_change, abs_change, measure)
) %>% 
  mutate(
    change_val   = if (use_relative) pct_change else abs_change,
    label_val    = if (use_relative)
      percent(change_val, accuracy = 1)
    else
      number(change_val, accuracy = 0.1),
    industry_fac = reorder_within(industry_text, change_val, measure),
    highlight    = if_else(industry_text == "Manufacturing", "Manufacturing", "Other")
  )

## ----------------- 5.  Faceted bar‑chart --------------------------------
gg <- ggplot(plot_df,
             aes(industry_fac, change_val, fill = highlight)) +
  geom_col(show.legend = FALSE) +
  # annotate the Manufacturing bar only
  geom_text(
    data = filter(plot_df, highlight == "Manufacturing"),
    aes(label = label_val),
    hjust = ifelse(plot_df$change_val[plot_df$highlight == "Manufacturing"] > 0, -0.1, 1.1),
    size = 2.5
  ) +
  coord_flip() +
  facet_wrap(~ measure, scales = "free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = c("Manufacturing" = "#2c3254", "Other" = "#ff8361")) +
  theme_classic(base_size = 14)

# y‑axis label + scale depend on metric type
if (use_relative) {
  gg <- gg +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(y = glue("Percent change, {base_year} \u2192 {comp_year}"))
} else {
  gg <- gg +
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    labs(y = glue("Change in rate points, {base_year} \u2192 {comp_year}"))
}

gg +
  labs(
    x = NULL,
    title = "Figure 4: Manufacturing Has Large Relative Job Openings Increase in Reopening",
    subtitle = "Relative change from 2019 to 2022, annual averages, percentage.",
    caption = "Source: BLS JOLTS. Annual averages (M13) used. Calculations by Mike Konczal"
  ) +
  theme(plot.title.position = "plot")

ggsave("graphics/g4_jolts_chart.png", width = 2800, height = 2190, dpi = 300, units = "px")