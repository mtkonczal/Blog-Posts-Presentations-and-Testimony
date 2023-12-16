pce <- prep_FRED_data("PCEPILFE")  %>% rename(DataValue = pcepilfe)

core <- pce %>%
  mutate(ThreeMonth = (DataValue/lag(DataValue,3))^4-1) %>%
  mutate(SixMonth = (DataValue/lag(DataValue,6))^2-1) %>%
  select(-DataValue) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA))

date_start = "2017-01-01"
date_end = "2019-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_core <- pce %>% filter(date == date_start | date == date_end) %>%
  mutate(change = DataValue/lag(DataValue,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(change)
pre_core <- as.numeric(pre_core)

one_month <- pce %>%
  mutate(time_length = "3-Month Change", p1A = (DataValue/lag(DataValue,1))^12-1) %>%
  filter(p1A > -0.02)

date_breaks <- sort(unique(pce$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 12)]

slide1 <- core %>% filter(date > "2016-12-01",
                          time_length == "3-Month Change") %>%
  left_join(one_month, by=c("date","time_length")) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  theme(legend.position = "none") +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text_repel(show.legend=FALSE, nudge_x = 75) +
  annotate("text", x = as.Date("2019-03-01"), y = 0.04, label = "Core PCE\n3-month change", size = 9, color = "#2D779C") +
  geom_col(aes(date, p1A), alpha=0.1, size=0, color="#2d779C", show.legend = FALSE)

slide1 <- slide1 +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)