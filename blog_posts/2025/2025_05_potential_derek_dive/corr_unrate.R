# ── 0.  Libraries ───────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(slider)   # for slide_dbl()
library(zoo)      # rollapplyr() – easier for 2-var correlations
library(showtext) # optional font for social media images
font_add_google("Roboto Condensed", "roboto"); showtext_auto()

# ── 1.  Data ────────────────────────────────────────────────────────────────
ny_unrate <- read_csv("data/nyfed_young_BA_unrate.csv") %>%      # your CSV
  clean_names() %>%                                              # janitor
  select(-x6, -x7) %>%                                           # drop junk cols
  inner_join(getFRED("JTSHIR"), by = "date") %>%                 # JOLTS hires
  rename(hires = jtshir) %>%                                     # clearer name
  arrange(date)                                                  # chronological

# ── 2.  4-year (48-month) rolling correlations ─────────────────────────────
window <- 48   # 4 years * 12 months

ny_roll <- ny_unrate %>%
  # keep only the columns we need
  select(date, hires, recent_graduates, all_workers, young_workers) %>%
  # for each unemployment column, run a rolling cor with hires
  mutate(across(
    .cols  = c(recent_graduates, all_workers, young_workers),
    .fns   = ~ rollapplyr(
      data.frame(h = hires, u = .x),
      width   = window,
      FUN     = function(mat) cor(mat[, "h"], mat[, "u"],
                                  use = "complete.obs"),
      by.column = FALSE,
      fill    = NA,        # leading NAs until the window is full
      align   = "right"
    ),
    .names = "corr_{.col}"
  )) %>%
  # reshape so each series gets its own row per date
  pivot_longer(
    cols   = starts_with("corr_"),
    names_to  = "series",
    names_pattern = "corr_(.*)",     # strip the 'corr_' prefix
    values_to = "value"
  ) %>%
  mutate(series = str_replace_all(series, "_", " "))   # nicer facet titles

# ── 3.  Plot ────────────────────────────────────────────────────────────────
line_col <- "#394b76"

corr_plot <- ny_roll %>%
  ggplot(aes(date, value, colour = series)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25)) +
  scale_colour_manual(values = c(
    "recent graduates" = "#bf3f3f",
    "young workers"    = "#1e8a8a",
    "all workers"      = line_col
  ), guide = guide_legend(title = NULL)) +
  labs(
    title = "Rolling 4-Year Correlation: JOLTS Hiring Rate vs. Unemployment Rates",
    subtitle = "Monthly data, January 2000 – March 2025 · 48-month window (right-aligned)",
    caption = "Source: NY Fed young worker unemployment data, BLS JOLTS (series JTSHIR) · @mtkonczal",
    x = NULL, y = "Correlation"
  ) +
  theme_classic(base_family = "roboto", base_size = 14) +
  theme(
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.key.width = unit(1.3, "cm")
  )

# ── 4.  Export for social media (Tweeter / slides) ──────────────────────────
ggsave("rolling_correlations.png", corr_plot,
       width = 10, height = 6, dpi = 320)
