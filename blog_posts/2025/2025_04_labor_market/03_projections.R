
graphic_indicators <- c("Government", "Leisure and hospitality",
                        "Manufacturing", "Private education and health services")

jobs <- ces %>% filter(seasonal == "S",
                       data_type_code == "01")

all_employ <- ces %>% filter(series_id == "CES0000000001") %>% select(date, total_jobs = value)

jobs <- jobs %>% filter(date >= "1990-01-01",
                        display_level == 2) %>%
  left_join(all_employ, by="date") %>%
  group_by(series_title) %>%
  mutate(diff_value = value/lag(value),
         diff_total = total_jobs/lag(total_jobs)) %>%
  ungroup() %>%
  mutate(percent_jobs = value/total_jobs,
         diff_percent = diff_value/diff_total)

jobs$projection <- logLinearProjection(jobs, "date", "percent_jobs", "1992-01-01", "2019-12-01", group_col = "series_title")
#jobs$projection2 <- logLinearProjection(jobs, "date", "percent_jobs", "2007-01-01", "2020-01-01", group_col = "series_title")

jobs %>%
  filter(year >= 1991) %>%
  filter(industry_name %in% graphic_indicators) %>%
  #  pivot_longer(c("percent_jobs", "projection"), names_to = "type", values_to = "value") %>%
  #  filter(industry_name == "Health care") %>%
  ggplot() +
  geom_line(aes(date, percent_jobs), color="#264653", size=1.2) +
  geom_line(aes(date, projection), color="#6a4c93", linetype = "dashed", size=1.2) +
  facet_wrap(~industry_name, scales="free") +
  theme_classic(base_size = 18) +
  labs(title = "Figure 3: Health Care Has Been Growing and is Near Trend",
       subtitle = "Percent of employment. Dashed line is a log-linear projection of 1992-2019.",
       caption = "Mike Konczal",
       x="",
       y="") + 
  scale_y_continuous(labels = percent) +
  theme(plot.title.position = "plot")