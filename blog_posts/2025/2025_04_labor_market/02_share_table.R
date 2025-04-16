
ces_data <- ces %>%
  filter(year(date) > 2010, date <= "2024-12-01") %>%
  mutate(data_type_code = as.numeric(data_type_code))

ces_last <- ces_data %>%
  filter(series_id == "CES0000000001") %>%
  filter(date == max(date)) %>%
  pull(value)
ces_2019 <- ces_data %>%
  filter(series_id == "CES0000000001") %>%
  filter(date == "2019-12-01") %>%
  pull(value)

jobs_chart <- ces_data %>%
  filter(seasonal == "S") %>%
  filter(data_type_code == 1) %>%
  filter(display_level <= 2) %>%
  group_by(industry_name) %>%
  reframe(
    change2024 = (value[date == max(date)] - value[date == max(date) %m-% months(12)])/12,
    change2019 = (value[date == "2019-12-01"] - value[date == "2018-12-01"])/12,
    share2024 = value[date == max(date)] / ces_last,
    share2019 = value[date == "2019-12-01"] / ces_2019,
    supersector_name = supersector_name[date == max(date)],
    display_level = display_level[date == max(date)],
  ) %>%
  ungroup() %>%
  mutate(change_share = share2024 - share2019)

ces_wages <- ces %>%
  filter(date == "2024-12-01",
         data_type_code == "03",
         seasonal == "S") %>%
  select(industry_name, avg_wages = value)


jobs_chart <- jobs_chart %>% left_join(ces_wages, by="industry_name")



jobs_chart %>%
  filter(!(industry_name %in% c("Service-providing", "Private service-providing"))) %>%
  mutate(groups = if_else(industry_name %in% c("Total nonfarm", "Total private", "Government"), "Total", "Industries")) %>%
  select(-change2019, -change2024) %>%
  mutate(order_total = factor(industry_name,
                              levels = c("Total nonfarm", "Total private", "Government"))) %>%
  arrange(groups,
          desc(change_share) * (groups == "Industries"),
          order_total) %>%
  select(-supersector_name, -display_level, -order_total) %>%
  gt(groupname_col = "groups") %>%
  row_group_order(group = c("Total", "Industries")) %>%
  tab_header(title = md(paste0("**Figure 2: Employment Shares by Industry**"))) %>%
  cols_label(
    share2024 = "2024 Share",
    share2019 = "2019 Share",
    change_share = "Change",
    industry_name = "",
    avg_wages = "2024 Wage"
  ) %>%
  tab_source_note(
    source_note = "BLS data, data from December for year. Mike Konczal"
  ) %>%
  opt_stylize(style = 6, color = "blue") %>%
  fmt_percent(columns = c(share2024, share2019, change_share), decimals = 2) %>%
  tab_spanner(
    label = "Percent of Employment",
    columns = c(share2024, share2019, change_share)
  ) %>%
  sub_missing(missing_text = "") %>%
  tab_style(
    style = cell_fill(color = "#C6EFCE"),
    locations = cells_body(columns = "change_share", rows = change_share > 0.0005)
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4CCCC"),
    locations = cells_body(columns = "change_share", rows = change_share < -0.0005)
  ) %>%
  gtsave(., filename="graphics/jobs_chart.png")
