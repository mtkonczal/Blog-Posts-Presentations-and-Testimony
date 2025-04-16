

# Plot Correlation Diffusion ----

#diffusion_industries <- read_csv("Good Scripts to Run/ces_diffusion_index_series/Manufacturing DI series-Table 1.csv")
diffusion_industries <- cesDiffusionIndex

unrate <- getFRED(c("UNEMPLOY","CLF16OV")) %>%
  mutate(unrate = unemploy/clf16ov) %>%
  select(date, unrate)
# Keep all years in the raw data
diffusion_jobs <- ces %>% filter(data_type_code == "01",
                                 industry_code %in% diffusion_industries$industry_code,
                                 seasonal == "S") %>%
  left_join(unrate, by="date") %>%
  select(date, total_jobs = value, unrate, industry_name)

# Step 1: Calculate 6-month changes using ALL years
job_changes <- diffusion_jobs %>%
  arrange(industry_name, date) %>%
  group_by(industry_name) %>%
  mutate(
    emp_change_6m = (total_jobs - lag(total_jobs, 6)) / lag(total_jobs, 6),
    unrate_change_6m = unrate - lag(unrate, 6)
  ) %>%
  ungroup()

# Step 2: Restrict to 2000–2019 for regression estimation
regression_sample <- job_changes %>%
  filter(year(date) >= 1991, year(date) <= 2019)

industry_coefs <- regression_sample %>%
  filter(!is.na(emp_change_6m), !is.na(unrate_change_6m)) %>%
  group_by(industry_name) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(emp_change_6m ~ unrate_change_6m, data = .x)),
    tidy_model = map(model, ~ tidy(.x))
  ) %>%
  unnest(tidy_model) %>%
  filter(term == "unrate_change_6m") %>%
  select(industry_name, estimate)


# Step 3: Classify sensitivity groups
quantiles <- quantile(industry_coefs$estimate, probs = c(0.20, 0.4, 0.6, 0.8))

#Fix
industry_coefs <- industry_coefs %>%
  mutate(
    sensitivity_group = case_when(
      estimate >= quantiles[4] ~ "Lowest",
      estimate >= quantiles[3] ~ "Low",
      estimate >= quantiles[2] ~ "Middle",
      estimate >= quantiles[1] ~ "High",
      TRUE ~ "Highest"
    )
  ) %>%
  mutate(sensitivity_group = factor(sensitivity_group, 
                                    levels = c("Lowest", "Low", "Middle", "High", "Highest")))



# Job growth by sensititvity: 1 month ----
ces %>% filter(seasonal == "S",
               data_type_code == "01",
               year >= 2011, date <= "2025-01-01") %>%
  inner_join(industry_coefs, by="industry_name") %>%
  group_by(series_title) %>%
  mutate(job_growth = (value - lag(value, 3))/3) %>%
  ungroup() %>%
  group_by(date, sensitivity_group) %>%
  reframe(job_growth = sum(job_growth)) %>%
  ungroup() %>%
  mutate(job_growth = if_else(year(date) %in% c(2020,2021), NA, job_growth)) %>%
  ggplot(aes(date, job_growth, color=sensitivity_group)) +
  geom_line(show.legend = FALSE) +
  theme_classic(base_size = 18) +
  facet_wrap(~sensitivity_group, scales = "free") +
  geom_hline(yintercept = 0) +
  labs(subtitle = "Three month average job gain by cyclical sensitivity. 2011-2024, 2020-2021 removed.",
       y = "",
       x = "",
       title = "Figure 5: The 2024 Labor Market Leveling Out in Cyclical Activity",
       caption = "252 Diffusion sub-industries correlated against unemployment changes. BLS. Mike Konczal") +
  theme(plot.title.position = "plot")



industry_coefs %>% arrange(estimate) %>%
  write_csv("output/cyclical_sensitivity.csv")