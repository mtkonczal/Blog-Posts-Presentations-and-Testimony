
# Main compared ----
MI_dates <- sort(unique(cps$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 12)]

remote_codes_government <- c("LNU0201B678", "LNU0201B66C", "LNU0201B66E")

remote_sector <- cps %>% filter(series_id %in% remote_codes_government) %>%
  mutate(value = value/100,
         series_title = str_replace_all(series_title, "Percent, Persons who teleworked, ", ""),
         series_title = str_replace_all(series_title, "wage and salary ", ""),
           series_title = str_replace_all(series_title, "incorporated ", "")) %>%
  filter(period != "M13")


remote_sector %>%
  mutate(dateTag = if_else(date >= max(date), value, NA)) %>%
  ggplot(aes(date, value, color = series_title)) +
  theme_classic(base_size = 18) +
  geom_line(size = 1.2) +
  geom_text(aes(date, dateTag, color=series_title, label = percent(dateTag)), nudge_x = 35, show.legend = FALSE, size = 5) +
  geom_point(aes(date, dateTag, color=series_title), size = 4, show.legend = FALSE) +
  geom_point(aes(date, value, color=series_title), size = 2, show.legend = FALSE) +
  scale_y_continuous(label = percent) +
    labs(title = "Figure 1: Federal Government Workers Working From Home Less Than The Private Sector",
         subtitle = "Percent of total at work. Persons who teleworked or worked at home, some or all hours. Not seasonally adjusted.",
         caption = "Current population survey. Series starts in October 2022. Mike Konczal",
         y = "",
         x = "") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates) +
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title.position = "plot",
        legend.position = "top",
        legend.title = element_blank()) 


# 1a Some all -----

some_all <- c("LNU0201B6FA", "LNU0201B788")

remote_sector2 <- cps %>% filter(series_id %in% some_all) %>%
  mutate(value = value/100,
         series_title = str_replace_all(series_title, "Percent, Persons who teleworked, ", ""),
         series_title = str_replace_all(series_title, "wage and salary ", ""),
         series_title = str_replace_all(series_title, "incorporated ", ""),
  series_title = str_replace_all(series_title, "government ", ""),
  series_title = str_replace_all(series_title, "teleworked ", "Teleworked "),
  series_title = str_replace_all(series_title, "Percent, Persons who ", "")) %>%
  filter(period != "M13")


remote_sector2 %>%
  mutate(dateTag = if_else(date >= max(date), value, NA)) %>%
  ggplot(aes(date, value, color = series_title)) +
  theme_classic(base_size = 18) +
  geom_line(size = 1.2) +
  geom_text(aes(date, dateTag, color=series_title, label = percent(dateTag)), nudge_x = 35, show.legend = FALSE, size = 5) +
  geom_point(aes(date, dateTag, color=series_title), size = 4, show.legend = FALSE) +
  geom_point(aes(date, value, color=series_title), size = 2, show.legend = FALSE) +
  scale_y_continuous(label = percent) +
  labs(title = "Figure 2: Both Fully and Partially Remote Workers Are Lower",
       subtitle = "Percent of total at work. Persons who teleworked or worked at home. Not seasonally adjusted.",
       caption = "Current population survey. Series starts in October 2022. Mike Konczal",
       y = "",
       x = "") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates) +
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title.position = "plot",
        legend.position = "top",
        legend.title = element_blank()) 


# Graphic 2: By Industry ------
plot_df <- cps %>% 
  filter(series_id %in% remote_codes_industry,          # keep the industries
         period != "M13") %>%                          # drop annual averages
  mutate(value = value / 100) %>%                      # to shares
  group_by(series_title) %>% 
  reframe(
    change = value[date == max(date)] -
      value[date == max(date) %m-% months(24)]
  ) %>% 
  ungroup() %>% 
  arrange(change) %>%                                  # sort for plot order
  mutate(
    series_title = str_replace_all(series_title, "Percent, Persons who teleworked, ", ""),
    label        = percent(change, accuracy = 0.1),    # text to show
    hjust_lbl    = ifelse(change > 0, -0.15, 1.15)     # text left/right
  )


ggplot(plot_df,
       aes(x = reorder(series_title, change), y = change)) +
  geom_col(fill = "#076fa2") +
  geom_text(aes(label = label, hjust = hjust_lbl),          # uses pre-built label
            size = 3, family = "sans", vjust = 0.5) +       # vjust keeps text centred
  coord_flip(clip = "off") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.20))) +
  labs(
    title    = "Remote work has risen most in industries",
    subtitle = "Percentage-point change in share of workers teleworking, last two years",
    caption  = "Source: IPUMS CPS microdata · @mtkonczal"
  ) +
  theme_economist() +
  theme(
    axis.title.y        = element_blank(),
    axis.title.x        = element_blank(),
    panel.grid.major.y  = element_blank(),      # no horizontal grid
    plot.title.position = "plot",
    plot.title          = element_text(size = 14, face = "bold"),
    plot.subtitle       = element_text(size = 11),
    plot.caption        = element_text(size = 8, hjust = 0),
    plot.margin         = margin(10, 30, 10, 10)
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

