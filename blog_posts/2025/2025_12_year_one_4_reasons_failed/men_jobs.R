library(tidyverse)
library(blsR)
library(tidyusmacro)
library(scales)
library(lubridate)
library(viridis)

# Set up your bls KEY and put it in your .Renviron using usethis::edit_r_environ()
bls_set_key(Sys.getenv("BLS_KEY"))


positive_color <- "#2c3254" # Bright blue
negative_color <- "#ff8361" # Pale violet

unrate <- get_n_series_table(
  c(
    "LNS13000000",
    "LNS11000000",
    "CES0500000021",
    "CES0000000001",
    "CES0000000010",
    "CES9091000001"
  ),
  api_key = bls_get_key(),
  start_year = 2011,
  end_year = 2025,
  tidy = TRUE
)

unrate <- unrate %>%
  mutate(
    LNS11000000 = as.numeric(LNS11000000),
    LNS13000000 = as.numeric(LNS13000000),
    unrate = LNS13000000 / LNS11000000,
    date = as.Date(paste0(year, "/", month, "/", 1)),
    diffusion = CES0500000021 / 100,
    ces = CES0000000001 - lag(CES0000000001, 1),
    federal = CES9091000001,
    nonfederal = CES0000000001 - federal,
    federal = CES9091000001 - lag(CES9091000001, 1),
    nonfederal = nonfederal - lag(nonfederal, 1)
  )


# Male jobs -----
gender <- unrate %>%
  mutate(
    diff_total = CES0000000001 - lag(CES0000000001, 1),
    diff_women = CES0000000010 - lag(CES0000000010, 1),
    diff_men = diff_total - diff_women,
    share_women = diff_women / diff_total,
    date = as.Date(paste0(year, "/", month, "/", 1))
  )

MI_dates <- date_breaks_n(gender$date, 6)

shares <- gender %>%
  mutate(
    period = case_when(
      year(date) %in% c(2023, 2024) ~ "2023–2024",
      year(date) >= 2025 ~ "2025",
      year(date) >= 2012 & year(date) <= 2019 ~ "2012-2019",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    women = sum(diff_women, na.rm = TRUE),
    total = sum(diff_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(share = ifelse(total == 0, NA_real_, women / total))

share_2324 <- shares %>% filter(period == "2023–2024") %>% pull(share)
share_2025 <- shares %>% filter(period == "2025") %>% pull(share)
share_0019 <- shares %>% filter(period == "2012-2019") %>% pull(share)

subtitle_text <- paste0(
  "CES Data, Total Nonfarm. Thousands. Women gained ",
  percent(share_2025, accuracy = 0.1),
  " of net job in 2025, YTD, vs ",
  percent(share_2324, accuracy = 0.1),
  " in 2023–2024."
)

plot_df <- gender %>%
  filter(year(date) >= 2023) %>%
  pivot_longer(
    c(diff_men, diff_women),
    names_to = "gender",
    values_to = "jobs"
  ) %>%
  mutate(gender = recode(gender, diff_men = "Men", diff_women = "Women"))


gender_description <- gender %>%
  filter(date >= "2025-05-01") %>%
  reframe(diff_men = sum(diff_men), diff_total = sum(diff_total))

gender_description <- paste0(
  "Men have gained ",
  gender_description$diff_men,
  " out of a total of ",
  gender_description$diff_total,
  " jobs since May 2025."
)


plot_df %>%
  ggplot(aes(x = date, y = jobs, fill = gender)) +
  geom_col(position = "stack") +
  geom_text(
    data = plot_df %>% filter(year(date) == 2025),
    aes(label = comma(round(jobs))),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.2
  ) +
  scale_fill_manual(values = c("Men" = "#2c3254", "Women" = "#ff8361")) +
  labs(
    title = gender_description,
    subtitle = subtitle_text,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "CES. Seasonally-adjusted. Total nonfarm. Mike Konczal, Economic Security Project."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 20)
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)
