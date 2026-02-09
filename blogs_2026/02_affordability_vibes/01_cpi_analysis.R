library(tidyverse)

# Load CPI data (seasonally adjusted price indices)
cpi <- read_csv("data/cpi_data.csv")

# --- Compute cumulative % change Dec 2019 to Dec 2025 ---
cpi_changes <- cpi %>%
  filter(date %in% as.Date(c("2019-12-01", "2025-12-01"))) %>%
  filter(item_display_level %in% 1:3) %>%
  pivot_wider(names_from = date, values_from = value, names_prefix = "val_") %>%
  rename(val_2019 = `val_2019-12-01`, val_2025 = `val_2025-12-01`) %>%
  filter(!is.na(val_2019), !is.na(val_2025)) %>%
  mutate(pct_change = (val_2025 / val_2019 - 1) * 100)

# Get All Items change for reference
all_items_change <- cpi %>%
  filter(
    item_name == "All items",
    date %in% as.Date(c("2019-12-01", "2025-12-01"))
  ) %>%
  pivot_wider(names_from = date, values_from = value) %>%
  mutate(pct_change = (`2025-12-01` / `2019-12-01` - 1) * 100) %>%
  pull(pct_change)

cat(
  "All Items cumulative inflation Dec 2019 to Dec 2025:",
  round(all_items_change, 1),
  "%\n"
)

cpi_changes <- cpi_changes %>%
  mutate(beats_all_items = pct_change > all_items_change)

# Tag essential vs discretionary
essential_keywords <- c(
  "Food at home",
  "Food away from home",
  "Shelter",
  "Rent of primary residence",
  "Owners' equivalent rent",
  "Motor vehicle insurance",
  "Motor vehicle maintenance",
  "Motor vehicle repair",
  "Motor fuel",
  "Gasoline",
  "Electricity",
  "Utility",
  "Energy services",
  "Household energy",
  "Water and sewer",
  "Hospital",
  "Physicians",
  "Medical care commodities",
  "Medical care services",
  "Prescription drug",
  "Health insurance",
  "Child",
  "Education",
  "Tuition",
  "Nonalcoholic beverages",
  "Garbage and trash",
  "Laundry",
  "Veterinarian",
  "Fuels and utilities",
  "Transportation services"
)

cpi_changes <- cpi_changes %>%
  mutate(
    category = if_else(
      str_detect(
        item_name,
        regex(paste(essential_keywords, collapse = "|"), ignore_case = TRUE)
      ),
      "Essential",
      "Discretionary"
    )
  )

write_csv(cpi_changes, "data/cpi_changes.csv")

# --- Graphic: Curated essentials that beat All Items inflation ---
# Use display_level 2 for meaningful categories, avoid ultra-granular items
# Keep essentials only, drop discretionary, allow one vet item
curated_items <- c(
  "Motor vehicle insurance", # display_level 2
  "Motor vehicle maintenance and repair", # display_level 2
  "Veterinarian services", # display_level 3 — one vet item
  "Energy services", # display_level 2
  "Household energy", # display_level 2
  "Food away from home", # display_level 2
  "Garbage and trash collection", # display_level 3
  "Food at home", # display_level 2
  "Owners' equivalent rent of residences", # display_level 2
  "Rent of primary residence", # display_level 2
  "Hospital and related services", # display_level 2
  "Shelter" # display_level 1
)

top_essentials <- cpi_changes %>%
  filter(item_name %in% curated_items, beats_all_items) %>%
  arrange(desc(pct_change))

g1 <- ggplot(
  top_essentials,
  aes(x = pct_change, y = reorder(item_name, pct_change))
) +
  geom_col(fill = "#D62828") +
  geom_text(
    aes(label = paste0(round(pct_change, 1), "%")),
    hjust = -0.1,
    size = 3.5
  ) +
  geom_vline(
    xintercept = all_items_change,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = all_items_change + 0.5,
    y = 1.5,
    label = paste0("All Items: ", round(all_items_change, 1), "%"),
    hjust = 0,
    size = 3.5
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Essential CPI Items That Beat Overall Inflation, Dec 2019 to Dec 2025",
    subtitle = "Cumulative percent change, seasonally adjusted",
    x = "Cumulative % Change",
    y = NULL,
    caption = "Source: BLS CPI, Author's Calculations"
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot")

ggsave(
  "graphics/cpi_essentials_beating_inflation.png",
  g1,
  width = 10,
  height = 7,
  dpi = 150
)

cat("CPI analysis complete. Graphics saved to graphics/\n")
