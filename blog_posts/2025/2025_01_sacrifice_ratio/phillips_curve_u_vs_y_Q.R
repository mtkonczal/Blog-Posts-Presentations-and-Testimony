source("getFRED.R")
library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)
library(viridis)

pc <- as_tibble(getFRED(c("GDPC1","GDPPOT","JCXFE"), rename_variables = c("real_gdp","potential_gdp","core_pce"), keep_all = FALSE)) %>%
  mutate(real_gdp_c = real_gdp/lag(real_gdp,1)-1,
         core_pce_c = core_pce/lag(core_pce,1)-1,
         change_potential = (real_gdp - potential_gdp)/potential_gdp,
         pandemic_years = year(date)>=2021,
         core_pce_c_YoY = core_pce/lag(core_pce,4)-1,
         real_gdp_c_YoY = real_gdp/lag(real_gdp, 4)-1
         )





pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(x = real_gdp_c, 
             y = core_pce_c, 
             color = pandemic_years)) +
  # Points
  geom_point() +
  # Add a linear model fit line
  geom_smooth(method = "lm", se = FALSE) +
  # Format axes as percent
  scale_x_continuous(labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  # Classic theme
  theme_classic(base_size = 16) +
  # Optional labeling of axes, title, etc.
  labs(
    title = "Figure 3: Level Shift, A Clear Way to See \u0394vt",
    subtitle="1991-Q4:2024, 2020 excluded as outliers. Pandemic is 2021-now. Lines are linear regressions on data subsets.",
    x = "Quarterly Real GDP Growth (%)",
    y = "Quarterly Core PCE Growth (%)",
    color = "Pandemic Years",
    caption = "Mike Konczal"
  ) +
  # Adjust theme as desired
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = c(0.25,0.8),
    plot.title.position = "plot"
  ) +
  scale_color_brewer(palette="Set1")

ggsave("g2.png", width = 12, height=6.75, dpi="retina")


# This is telling follow-up graphic, watching the move alongside potential
pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(change_potential, core_pce_c_YoY, color=pandemic_years)) +
  geom_path(data = . %>% filter(year(date) > 2020), aes(change_potential, core_pce_c_YoY, color=pandemic_years)) +
  geom_point() +
  # Format axes as percent
  scale_x_continuous(labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  # Classic theme
  theme_classic(base_size = 16) +
  # Optional labeling of axes, title, etc.
  labs(
    title = "Inflation Fell As GDP Grew Faster Against Potential",
    subtitle="Q1:1991-Q4:2024, 2020 excluded as outliers. Pandemic is 2021-now. Y-Axis is Real GDP minus CBO potential estimate.",
    x = "Year-Over-Year Real GDP Growth - CBO Potential (% of Potential)",
    y = "Year-Over-Year Core PCE Growth (%)",
    color = "Pandemic Years",
    caption = "Mike Konczal"
  ) +
  # Adjust theme as desired
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = c(0.25,0.8),
    plot.title.position = "plot"
  ) +
  scale_color_brewer(palette="Set1") +
  geom_text(
    data = pc %>% filter(pandemic_years) %>%   mutate(date_label = paste0("Q",(month(date)-1)/3+1,"\n",year(date))) %>%
      filter(date == max(date) | date == min(date)),
    aes(label = date_label),  # or however you want to format the date
    vjust = -0.5,   # tweak vertical placement
    hjust =  0.5,   # tweak horizontal placement
    size  = 6,
    show.legend = FALSE
  )

ggsave("g3.png", width = 12, height=6.75, dpi="retina")
