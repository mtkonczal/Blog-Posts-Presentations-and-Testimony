
ces_data <- ces %>%
  filter(year == 2024, period == "M13") %>%
  mutate(data_type_code = as.numeric(data_type_code))

ces_last <- ces_data %>%
  filter(series_id == "CEU0000000001") %>%
  pull(value)

jobs_chart <- ces_data %>%
  filter(display_level <= 2, data_type_code == 1) %>%
  group_by(industry_name) %>%
  reframe(
    total_jobs = value,
    series_id = series_id
  )


jobs_chart <- jobs_chart %>%
  mutate(chart_type = case_when(
    industry_name %in% c("Total nonfarm", "Goods-producing", "Private service-providing", "Total private") ~ "Total",
    TRUE ~ "Goods and Services"
  )) %>%
  mutate(chart_type = factor(chart_type, levels = c("Total", "Goods and Services"))) %>%
  filter(industry_name != "Service-providing") 

wage_data <- ces_data %>%
  filter((display_level <= 2 & data_type_code == 3)) %>%
  group_by(industry_name) %>%
  summarize(last_wages = value) %>%
  ungroup()

jobs_chart2 <- jobs_chart %>%
  mutate(percent_jobs = total_jobs/ces_last) %>%
  left_join(wage_data, by = "industry_name") %>%
  #left_join(industry_timeline, by = "series_id") %>%
  mutate(industry_name = if_else(industry_name == "Total nonfarm", "All jobs", industry_name)) %>%
  arrange(last_wages)

cum_wages <- jobs_chart2 %>%
  filter(chart_type == "Goods and Services", industry_name != "Government") %>%
  mutate(percent_jobs2 = percent_jobs/sum(percent_jobs),
         dist = cumsum(percent_jobs2)) %>%
  select(industry_name, dist)
         
jobs_chart2 %>%
  left_join(cum_wages, by="industry_name") %>%
  select(-series_id) %>%
  gt(groupname_col = "chart_type") %>%
  row_group_order(groups = c("Total", "Goods and Services")) %>%
  tab_header(title = "Figure 5: Slight Majority of Jobs Pay More Than Manufacturing",
             subtitle = "Values are average within 2024. Jobs in thousands.") %>%
  cols_label(
    industry_name = "",
    total_jobs = "Total Jobs",
    percent_jobs = "% of Jobs",
    last_wages = "Average Hourly\nEarnings",
    dist = "Culminative Private\nWage Distribution"
  ) %>%
  tab_source_note(
    source_note = "BLS data, author's calculations. Average values in 2024, no government pay data. Mike Konczal."
  ) %>%
  opt_stylize(style = 6, color = "blue") %>%
  fmt_percent(columns = c(percent_jobs, dist),
              decimals = 1) %>%
  sub_missing(missing_text = "") %>%
  fmt_currency(columns = last_wages,
               currency = "dollar") %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(
      rows = industry_name %in% c("Manufacturing","Total private")
    )
  ) %>%
  fmt_number(                # <-- add this
    columns  = total_jobs,
    decimals = 0,            # no decimal places
    sep_mark = ","           # use commas as thousands separator
  )