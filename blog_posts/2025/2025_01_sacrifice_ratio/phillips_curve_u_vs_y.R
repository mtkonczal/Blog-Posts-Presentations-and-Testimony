source("getFRED.R")
library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)
library(viridis)

pc <- as_tibble(getFRED(c("UNEMPLOY","CLF16OV","PCEPILFE"),
                        rename_variables = c("u_level", "labor_force","core_pce"),
                        keep_all = FALSE)) %>%
  inner_join(getFRED(c("GDPC1","GDPPOT"), rename_variables = c("real_gdp","potential_gdp")) %>%
               mutate(date = date %m+% months(2)), by="date") %>%
  mutate(real_gdp_c = real_gdp/lag(real_gdp,1)-1,
         core_pce_c = core_pce/lag(core_pce,1)-1,
         change_potential = (real_gdp - potential_gdp)/potential_gdp,
         pandemic_years = year(date)>=2021,
         u = u_level/labor_force,
         change_u = u-lag(u,1),
         core_pce_c_YoY = core_pce/lag(core_pce,4)-1,
         real_gdp_c_YoY = real_gdp/lag(real_gdp, 4)-1
         )

pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(u, (1+core_pce_c)^4-1)) +
  geom_point() +
  theme_classic() +
  geom_smooth()


pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(u, (1+core_pce_c)^4-1, color=pandemic_years)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm")

# This is the first graphic - a clear way to draw deltavt
pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(real_gdp_c, core_pce_c, color=pandemic_years)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm")

# YoY version of above
pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(real_gdp_c_YoY, core_pce_c_YoY, color=pandemic_years)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm")

pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(change_potential, core_pce_c, color=pandemic_years)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm")


# This is telling follow-up graphic, watching the move alongside potential
pc %>%
  filter(year(date) >= 1991,
         year(date) != 2020) %>%
  ggplot(aes(change_potential, core_pce_c_YoY, color=pandemic_years)) +
  geom_path(data = . %>% filter(year(date) > 2020), aes(change_potential, core_pce_c_YoY, color=pandemic_years)) +
  geom_point() +
  theme_classic()

pc %>%
  ggplot() +
  geom_line(aes(date, log(real_gdp)), color="black") +
  geom_line(aes(date, log(potential_gdp)), color="red")


##### Okun's Law ####

unrate <- as_tibble(getFRED(c("UNEMPLOY","CLF16OV"),
                        rename_variables = c("u_level", "labor_force"),
                        keep_all = FALSE)) %>%
  mutate(u = u_level/labor_force)


pc <- as_tibble(getFRED(c("gdpc1","PCEPILFE"),
                        rename_variables = c("real_gdp","core_pce"),
                        keep_all = FALSE)) %>%
  mutate(date = date %m+% months(2),
         real_gdp_c = (real_gdp/lag(real_gdp,1))^4-1,
         core_pce_c = core_pce/lag(core_pce,1)-1,
         pandemic_years = year(date)>=2021) %>%
  left_join(unrate, by="date") %>%
  mutate(change_u = u-lag(u,1))

tail(pc)


pc %>%
  filter(year(date) != 2020) %>%
  ggplot(aes(change_u, real_gdp_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_path(data = pc %>%
                  filter(year(date) >= 2022), aes(change_u, real_gdp_c), color="purple", size = 2) +
  theme_classic() +
  geom_label_repel(data = pc %>% filter(date == max(date) | date == "2022-03-01" | date == max(date) %m-% months(3)), aes(change_u, real_gdp_c, label=format(date, "%b\n%Y")),
                   nudge_y = 0.04) +
  labs(title = "Traveling The Wrong Way Along Okun's 'Law'",
       subtitle = "Change in Unemployment Rate vs Real GDP Growth Rate, Quarterly Annualized, Q1:1959 to Q3:2024",
       y="Real GDP Growth Rate",
       x="Change in Unemployment Rate",
       caption = "2020 removed as outliers.") +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)




##### Okun's Law Great Recession ####
pc %>%
  filter(year(date) != 2020) %>%
  ggplot(aes(change_u, real_gdp_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_path(data = pc %>%
              filter(year(date) >= 2009 & year(date) <= 2012), aes(change_u, real_gdp_c), color="purple", size = 2) +
  theme_classic() +
  geom_label(data = pc %>% filter(date == "2009-01-01" | date == "2013-01-01"), aes(change_u, real_gdp_c, label=format(date, "%b\n%Y"))) +
  labs(title = "Traveling Sideways Along Okun's 'Law'",
       subtitle = "Change in Unemployment Rate, Real GDP Growth Rate, Quarterly, Q1:1959 to Q3:2024",
       y="Change in Real GDP Growth Rate",
       x="Change in Unemployment Rate",
       caption = "2020 removed as outliers.") +
  theme(plot.title.position = "plot")








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
    subtitle="1991-Q3:2024, 2020 excluded as outliers. Pandemic is 2021-now. Lines are linear regressions on data subsets.",
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
    title = "Figure 4: Inflation Fell As GDP Grew Faster",
    subtitle="Q1:1991-Q3:2024, 2020 excluded as outliers. Pandemic is 2021-now. Y-Axis is Real GDP minus CBO potential estimate.",
    x = "Year-Over-Year Real GDP Growth - CBO Potential (%)",
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
    data = pc %>% filter(pandemic_years) %>%
      filter(date == max(date) | date == min(date)),
    aes(label = format(date, "%b\n%Y")),  # or however you want to format the date
    vjust = -0.5,   # tweak vertical placement
    hjust =  0.5,   # tweak horizontal placement
    size  = 6,
    show.legend = FALSE
  )