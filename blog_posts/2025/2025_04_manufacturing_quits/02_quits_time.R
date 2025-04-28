make_jolts_graphic <- function(jolts,
                               base_year           = 2019,
                               use_case            = c("relative", "absolute", "level"),
                               highlight_industries = c("Manufacturing",
                                                        "Leisure and hospitality"),
                               highlight_cols       = c("Manufacturing"            = "#2c3254",  # red
                                                        "Leisure and hospitality" = "#ff8361")  # blue
) {
  
  use_case <- match.arg(use_case)
  
  # ---------------- 1.  Base-year levels ---------------------------------
  base <- jolts %>%
    filter(period == "M13", year == base_year,
           display_level.x == 2,
           dataelement_code == "QU",
           ratelevel_code   == "R",
           state_text       == "Total US") %>%
    select(industry_text, base_level = value)
  
  # ---------------- 2.  Build full monthly series ------------------------
  change_flows <- jolts %>%
    filter(display_level.x == 2,
           seasonal == "S",
           dataelement_code == "QU",
           ratelevel_code   == "R",
           state_text       == "Total US") %>%
    left_join(base, by = "industry_text") %>%
    group_by(industry_text) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      change_relative = value / base_level - 1,
      change_absolute = value/100 - base_level/100,
      value = value/100,
      change_relative_ma6 = slide_dbl(change_relative, mean, .before = 5, .complete = TRUE),
      level = slide_dbl(value, mean, .before = 5, .complete = TRUE),
      change_absolute_ma6 = slide_dbl(change_absolute, mean, .before = 5, .complete = TRUE)
    ) %>%
    ungroup()
  
  # ---------------- 3.  Choose series & labels ---------------------------
  var_to_plot <- switch(use_case,
                        relative = "change_relative_ma6",
                        absolute = "change_absolute_ma6",
                        level    = "level")
  
  y_lab <- switch(use_case,
                  relative = "% change from base year (6-mo avg.)",
                  absolute = "pp change from base year (6-mo avg.)",
                  level    = "Quits rate")
  
  plot_data <- change_flows %>%
    filter(!is.na(.data[[var_to_plot]]),
           year(date) >= 2011)
  
  # pick label dates: manufacturing at 2022-07-01, leisure one year earlier
  label_dates <- c("Manufacturing"            = as.Date("2022-07-01"),
                   "Leisure and hospitality"  = as.Date("2013-09-01"))
  
  label_pts <- purrr::map_dfr(names(label_dates), \(ind) {
    plot_data %>%
      filter(industry_text == ind, date == label_dates[[ind]])
  })
  label_pts <- label_pts %>%
    filter(industry_text %in% highlight_industries)
  
  # ---------------- 4.  Plot ---------------------------------------------
  ggplot() +
    # background (non-highlight) lines
    geom_line(
      data = filter(plot_data, !industry_text %in% highlight_industries),
      aes(date, .data[[var_to_plot]], group = industry_text),
      colour = "grey50", alpha = 0.4, linewidth = 0.9
    ) +
    # highlight lines
    geom_line(
      data = filter(plot_data, industry_text %in% highlight_industries),
      aes(date, .data[[var_to_plot]], colour = industry_text),
      linewidth = 1.2
    ) +
    scale_colour_manual(values = highlight_cols, guide = "none") +
    # inline text labels
    geom_text(
      data = label_pts,
      aes(date, .data[[var_to_plot]], label = industry_text, colour = industry_text),
      hjust = -0.1, vjust = -0.6, fontface = "bold"
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = "Placeholder",
      subtitle = "Placeholder",
      x = NULL, y = y_lab,
      caption = "Two-digit industries shown are: Mining and logging, Construction, Manufacturing, Trade, transportation, and utilities, Information, Financial activities, Professional and business services,\nPrivate education and health services, Leisure and hospitality, Other services, Federal government, State and local government.\nMike Konczal"
    ) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.margin = margin(10, 20, 10, 10),
          plot.title.position = "plot",
          axis.title=element_text(size=8),
          plot.caption = element_text(size=6)
          )
}



make_jolts_graphic(jolts, use_case = "relative", highlight_industries = "Manufacturing") +
  labs(title = "Figure 2: Manufacturing Had the Highest Relative Increase in Quits During Pandemic",
       subtitle = "JOLTS quit rate by two-digit industries, six-month average, relative change compared to 2019 annual.",
       y = "6m average % change from own 2019 annual rate") +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10))
ggsave("graphics/g2_relative.png", width = 2190, height = 1200, dpi = 300, units = "px")


make_jolts_graphic(jolts, use_case = "absolute") +
  labs(title = "Figure 3: Manufacturing Rivals Hospitality For Highest Absolute Increase in Quits During Pandemic",
       subtitle = "JOLTS quit rate by two-digit industries, six-month average, absolute change compared to 2019 annual.",
       y = "6m average minus 2019 own average quit rate") +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10))
ggsave("graphics/g3_absolute.png", width = 2190, height = 1200, dpi = 300, units = "px")

