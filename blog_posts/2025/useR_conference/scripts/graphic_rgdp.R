# ------------------------------------------------------------

# ------------------------------------------------------------
# 2.  Real GDP growth  (% at annual rate, table 1.1.1)
# ------------------------------------------------------------

df <- getFRED(c("A191RL1Q225SBEA","PB0000031Q225SBEA"), rename_variables = c("real_gdp", "real_private_final_demand")) %>%
  mutate(date = date %m+% months(2))

df <- df %>%
  pivot_longer(real_gdp:real_private_final_demand, names_to = "series", values_to = "value") %>%
  mutate(series = case_when(
           series == "real_gdp" ~ "Real GDP",
           series == "real_private_final_demand" ~ "Final Sales to Private Domestic Purchasers",
           TRUE ~ "Error"
         ))
# ------------------------------------------------------------
# 4.  Merge, keep quarters ≥ 2023, prep labels + highlight
# ------------------------------------------------------------
plot_df <- df %>%
  filter(year(date) >= 2023) |> 
  mutate(series    = factor(series,
                            levels = c("Real GDP",
                                       "Final Sales to Private Domestic Purchasers")),
         value_pct = value/100,
         label     = percent(value_pct, accuracy = 0.1),
         label = str_remove_all(label, "%"),
         text_y    = value_pct/2,                         # mid-bar
         fill_flag = if_else(date == max(date),           # latest quarter
                             "highlight", "normal"))

# ------------------------------------------------------------
# 5.  2023-24 CAGR for each series
# ------------------------------------------------------------
cagr_23_24 <- function(df) {
  df |> filter(year(date) %in% 2023:2024) |>
    mutate(qtr_factor = (1 + value/100)^(1/4)) |>
    summarise(cagr = prod(qtr_factor)^(1/2) - 1) |>
    pull(cagr)
}

cagr_tbl <- tribble(
  ~series, ~cagr,
  "Real GDP",                       cagr_23_24(df %>% filter(series == "Real GDP")),
  "Final Sales to Private Domestic Purchasers",  cagr_23_24(df %>% filter(series == "Final Sales to Private Domestic Purchasers"))
) |>
  mutate(series = factor(series,
                         levels = c("Real GDP",
                                    "Final Sales to Private Domestic Purchasers")))

# ------------------------------------------------------------
# 6.  Colours
# ------------------------------------------------------------
bar_col       <- "#394b76"
highlight_col <- "#ff8361"
trend_col     <- "#c81d25"

# ------------------------------------------------------------
# 7.  Plot
# ------------------------------------------------------------
rgdp_graphic <- ggplot(plot_df, aes(date, value_pct, fill = fill_flag)) +
  geom_col() +
  geom_text(aes(y = text_y, label = label),
            colour = "white") +
  geom_hline(data = cagr_tbl,
             aes(yintercept = cagr),
             colour = trend_col, linewidth = 1) +
  geom_text(data = cagr_tbl,
            aes(x = max(plot_df$date) + 15,
                y = cagr,
                label = glue("Avg 2023-24:\n{percent(cagr, 0.1)}")),
            hjust = 1, vjust = -0.4,
            colour = trend_col,
            inherit.aes = FALSE) +
  
  scale_fill_manual(values = c(normal = bar_col,
                               highlight = highlight_col),
                    guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.5),
                     expand = expansion(mult = c(0, .05))) +
  facet_wrap(~series, ncol = 2) +
  
  labs(
       subtitle = "Quarterly growth annualized.",
       caption  = "Source: BEA NIPA Tables 1.4.1.",
       x = NULL, y = NULL) +
  theme_chartbook()
  