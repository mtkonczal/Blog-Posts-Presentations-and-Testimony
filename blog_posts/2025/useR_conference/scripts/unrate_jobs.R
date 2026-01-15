
bls_api <- "996d4e4af85f43f3ac301805891cbf6e"
bls_set_key(bls_api)


positive_color <- "#4594ff" # Bright blue
negative_color <- "#BA68C8" # Pale violet

unrate <- get_n_series_table(
  c("LNS13000000", "LNS11000000", "CES0500000021", "CES0000000001"),
  api_key = bls_get_key(),
  start_year = 2019,
  end_year = 2025,
  tidy = TRUE
)

unrate <- unrate %>%
  mutate(
    unrate = LNS13000000 / LNS11000000,
    date = as.Date(paste0(year, "/", month, "/", 1)),
    diffusion = CES0500000021 / 100,
    ces = CES0000000001 - lag(CES0000000001, 1)
  )

MI_dates <- sort(unique(unrate$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 6)]


# Unemployment Rate ----
unrate_graphic <- unrate %>%
  filter(date > max(date) %m-% months(24)) %>%
  mutate(dateTag = if_else(date >= max(date) %m-% months(1), round(unrate, 4), NA)) %>%
  ggplot(aes(date, unrate, label = percent(dateTag))) +
  geom_line(size = 1.2, color = positive_color) +
  geom_text(aes(date, dateTag), nudge_x = 25, color = positive_color) +
  geom_point(aes(date, dateTag), size = 4, color = positive_color) +
  scale_y_continuous(label = percent) +
  theme_classic() +
  labs(
    subtitle = "Unemployment Rate, Manually Calculated"
  ) +
  theme_chartbook() +
  scale_chartbook_colors()

jobs_gained <- unrate %>% filter(date == max(date)) %>% pull(ces)
max_date<- unrate %>% filter(date == max(date)) %>% pull(date)
  
  # CES Jobs Gained ----
  jobs_gained_graphic <- unrate %>%
  mutate(
    fill_tag = date == max(date)
  ) %>%
  filter(date >= "2023-01-01") %>%
  ggplot(aes(date, ces, fill = fill_tag)) +
  geom_col(size = 0, show.legend = FALSE) +
  labs(
    subtitle = paste0("Monthly Jobs Gained in CES. ", jobs_gained, ",000 in ", format(max_date, "%B, %Y"),".")
  ) +
  theme_chartbook() +
  scale_chartbook_fill()
