

remote_codes_industry <- c("LNU0201B67C",
                           "LNU0201B67E",
                           "LNU0201B680",
                           "LNU0201B684",
                           "LNU0201B688",
                           "LNU0201B68A",
                           "LNU0201B686",
                           "LNU0201B68C",
                           "LNU0201B68E",
                           "LNU0201B690",
                           "LNU0201B692",
                           "LNU0201B694",
                           "LNU0201B682",
                           "LNU0201B696",
                           "LNU0201B698",
                           "LNU0201B69A",
                           "LNU0201B69C",
                           "LNU0201B69E",
                           "LNU0201B6A0",
                           "LNU0201B6A2",
                           "LNU0201B6A4",
                           "LNU0201B6A6",
                           "LNU0201B6A8",
                           "LNU0201B6AC",
                           "LNU0201B6AA",
                           "LNU0201B6AE",
                           "LNU0201B6B0",
                           "LNU0201B6B2",
                           "LNU0201B6B4",
                           "LNU0201B6B6",
                           "LNU0201B6B8",
                           "LNU0201B6BA",
                           "LNU0201B6BC")


# ----------------------------------------
# 1. Build the data ----------------------
# ----------------------------------------
pad <- 0.001   # 0.5 ppt of extra space; tweak to taste

plot_df <- cps %>% 
  filter(series_id %in% remote_codes_industry,
         period != "M13") %>%                      # drop annual averages
  mutate(value = value / 100) %>%                 # to shares
  group_by(series_title) %>% 
  reframe(
    date = date,
    m3avg = (value + lag(value, 1) + lag(value, 2)) / 3) %>%
  ungroup() %>%
  group_by(series_title) %>%
  mutate(change = m3avg[date == max(date)] -
      m3avg[date == max(date) %m-% months(24)]
  ) %>% 
  ungroup() %>% 
  distinct(series_title, .keep_all = TRUE) %>%
  arrange(change) %>% 
  mutate(
    series_title = str_replace_all(series_title,
                                   "Percent, Persons who teleworked, ", ""),
    label   = percent(change, accuracy = 0.1),
    y_lbl   = change + pad                     # << new position for text
  )

# ----------------------------------------
# 2. Plot --------------------------------
# ----------------------------------------
ggplot(plot_df,
       aes(x = reorder(series_title, change), y = change)) +
  geom_col(fill = "#076fa2") +
  geom_text(aes(y = y_lbl, label = label),     # use the offset position
            hjust = 0, vjust = 0.5,
            size  = 5, family = "sans") +
  coord_flip(clip = "off") +
  scale_y_continuous(labels  = percent_format(accuracy = 1),
                     expand  = expansion(mult = c(0, 0.25))) +  # room for labels
  theme_classic(base_size = 18) +
  labs(
    title    = "Figure 3: Remote Work Has Risen Across in Private Industries",
    subtitle = "Percentage-point change in share of workers teleworking, 3-month average, April 2025 minus April 2023.",
    caption  = "Source: CPS. Mike Konczal"
  ) +
  theme(
    axis.title        = element_blank(),
    panel.grid.major.y= element_blank(),
    plot.title.position = "plot",
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 18),
    plot.caption      = element_text(size = 8, hjust = 0),
    plot.margin       = margin(10, 30, 10, 10)
  )







# Facet in footnotes ----
plot_df <- cps %>% 
  filter(series_id %in% remote_codes_industry,          # keep the industries
         period != "M13") %>%                          # drop annual averages
  mutate(value = value / 100) %>%                      # to shares
  mutate(
    series_title = str_replace_all(series_title, "Percent, Persons who teleworked, ", "")
  )


ggplot(plot_df,
       aes(date, value)) +
  geom_line(color = "#076fa2", size =1.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Remote work has risen most in industries",
    subtitle = "Percentage-point change in share of workers teleworking, last two years",
    caption  = "Source: CPS · @mtkonczal"
  ) +
  facet_wrap(~series_title, scales = "free") +
  theme_classic() +
  theme(
    axis.title.y        = element_blank(),
    axis.title.x        = element_blank(),
    panel.grid.major.y  = element_blank(),      # no horizontal grid
    plot.title.position = "plot",
    plot.title          = element_text(size = 14, face = "bold"),
    plot.subtitle       = element_text(size = 11),
    plot.caption        = element_text(size = 8, hjust = 0)
  )

