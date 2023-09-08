
#### Jolts Graphic ####

# Create the new variables
pc_analysis <- pc_data %>%
  mutate(
    core_pce_changeA = (core_pce/lag(core_pce,3))^4 - 1,
    core_pce_changeA = core_pce_changeA*100,
  ) %>%
  filter(!is.na(core_pce_changeA))

pc_analysis$nrou <- na.locf(pc_analysis$nrou, na.rm = FALSE)
pc_analysis$gdp <- na.locf(pc_analysis$gdp, na.rm = FALSE)
pc_analysis$FRB_exp <- na.locf(pc_analysis$FRB_exp, na.rm = FALSE)
pc_analysis$unrate_slack = pc_analysis$unrate - pc_analysis$nrou

pc_analysis$v_u_ratio = pc_analysis$long_openings/pc_analysis$unrate
pc_analysis$v_u_ratio_star = pc_analysis$long_openings/pc_analysis$unrate - pc_analysis$long_openings/pc_analysis$nrou

start_month <- month(max(pc_analysis$date))
quarters <- ((seq(start_month, start_month + 9, by=3) - 1) %% 12) + 1

pc_analysis <- pc_analysis %>% filter(month(date) %in% quarters)

model_uv <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + expinf5yr +
                 v_u_ratio, data = pc_analysis[pc_analysis$date<"2020-01-01",])

model_u <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + expinf5yr +
                unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01",])

pc_analysis$predicted_1980_2019_vu <- predict(model_uv, newdata = pc_analysis)
pc_analysis$predicted_1980_2019_u <- predict(model_u, newdata = pc_analysis)

pc_analysis %>% filter(year(date)>1980) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation")) +
  geom_line(aes(y = predicted_1980_2019_vu, color = "Predicted Inflation - v/u - v/u*")) +
  geom_line(aes(y = predicted_1980_2019_u, color = "Predicted Inflation - u - u*")) +
  #geom_line(aes(y = predicted_1970_2019_vu, color = "1970s Predicted Inflation n/u")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation - v/u Phillips Curve", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is quarterly 1982-2019, quarters starting in July, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (v/u) or (u-u*)$)"),
       caption="Cleveland Fed 5-year expected inflation used for expectations. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation - u - u*" = "red","Predicted Inflation - v/u - v/u*" = "purple")) +
  theme_minimal() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot")


pc_analysis %>%
  filter(year(date)>1984 & year(date)<2020) %>%
  select(unrate_slack) %>%
  summarize(mean = mean(unrate_slack), median = median(unrate_slack))
## Larry Ball stuff
pc_data
pc_analysis2 <-
  pc_data %>%
  filter(!is.na(median_cpi)) %>%
  select(-mich, -gdp, -nrou) %>%
  mutate(v_u_ratio = long_openings/unrate,
         avg_v_u = v_u_ratio + lag(v_u_ratio,1) + lag(v_u_ratio,2) + lag(v_u_ratio,3),
         avg_v_u = avg_v_u/4) %>%
  mutate(median_cpi_unA = median_cpi/100 + 1,
         median_cpi_unA = median_cpi_unA^(1/12),
         median_cpi_index = cumprod(median_cpi_unA),
         median_cpi_unA = median_cpi_unA/lag(median_cpi_unA,12)-1) %>%
  mutate(core_inflation_gap = expinf5yr - median_cpi_unA) %>%
  select(date, avg_v_u, core_inflation_gap, median_cpi_unA, median_cpi) %>%
  filter(!is.na(avg_v_u)) %>%
  mutate(pandemic = year(date)>=2020) %>%
  mutate(pandemic_value = if_else(pandemic, avg_v_u, as.numeric(NA)),
         last_value = if_else(date==max(date), format(date, "%B\n%Y"), as.character(NA))) %>%
  ggplot(aes(avg_v_u, median_cpi, color=pandemic, label=last_value)) + geom_point() +
  geom_path(aes(pandemic_value, median_cpi)) + geom_text_repel(show.legend = FALSE) +
  labs(title="What happened?", subtitle="Figure 3 from the 'Scariest Economics Paper of 2022' (-Jason Furman). I have not done the fancy adjustments yet. Monthly.") +
  theme(legend.position = "NONE")

