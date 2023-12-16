cpi_demand <- read_csv("data/demand_sensitive_index.csv")


date_breaks <- sort(unique(cpi_demand$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 12)]

slide_demand <- cpi_demand %>% filter(date > "2016-12-01") %>%
  ggplot(aes(date, changeWA)) + geom_line(size=1.2, color="#2D779C") +
  labs(x="", y="") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  theme(legend.position = "none") +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  annotate("text", x = as.Date("2019-03-01"), y = 0.085, label = "Demand Sensitive\nNon-Housing CPI", size = 9, color = "#2D779C")

slide_demand <- slide_demand +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)