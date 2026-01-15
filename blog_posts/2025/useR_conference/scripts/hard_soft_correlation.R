
df <- getFRED(c("UNEMPLOY", "CLF16OV")) %>%
  mutate(unrate = unemploy/clf16ov) %>%
  select(date, unrate) %>%
  left_join(mich_cs, by="date") %>%
  filter(date >= "1978-01-01") %>%
  select(-Month, -YYYY) %>%
  rename(sentiment_index = ICS_ALL)


max_lag <- 18                # months to push UNRATE backward / forward

cor_tbl2 <- tibble(k = -max_lag:max_lag) |>
  mutate(correlation = map_dbl(k, \(k) {
    shifted_unrate <- if (k >= 0) {
      lead(df$unrate,  n = k)          # +k → UNRATE in the FUTURE
    } else {
      lag(df$unrate,  n = abs(k))      # −k → UNRATE in the PAST
    }
    cor(shifted_unrate, df$sentiment_index, use = "complete.obs")
  }))

# ---- Plot ---------------------------------------------------------------
corr_graphic <- ggplot(cor_tbl2, aes(k, correlation)) +
  theme_chartbook() +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +   # contemporaneous
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = seq(from = -max_lag, to = max_lag, by=2)) +
  theme(axis.title = element_text()) +
  labs(
#    title    = "Consumer Sentiment Shifts Most Correlated with Unemployment 10 Months Out",
    subtitle = "Correlation between unemployment and index of consumer sentiment. Monthly, 1978-2025.",
    x        = "Number of months unemployment is shifted",
    y        = ""
  )
