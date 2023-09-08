library(tidyverse)
library(distill)
library(stargazer)
library(knitr)
library(scales)
library(lubridate)
library(kableExtra)

chart1 <- read_csv("data/chart1.csv")
chart2 <- read_csv("data/chart2.csv")
pce_supply <- read_csv("data/pce_supply.csv")
pc_analysis <- read_csv("data/pc_analysis.csv")
cyclical_pce <- read_csv("data/cyclical_pce.csv")

#Graphic 1
pc_analysis_1991 <- pc_analysis %>% filter(!is.na(FRB_post1991))
model <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + FRB_exp +
              unrate_slack, data = pc_analysis_1991[pc_analysis_1991$date<"2020-01-01",])
summary(model)

model2 <- lm(core_pce_changeA ~ lag(core_pce_changeA, 1) + lag(core_pce_changeA, 2) + FRB_exp +
               unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01",])

pc_analysis$predicted_1980_2019 <- predict(model, newdata = pc_analysis)
pc_analysis$predicted_1970_2019 <- predict(model2, newdata = pc_analysis)

date_breaks <- sort(unique(pc_analysis$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 24)]

pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation")) +
  geom_line(aes(y = predicted_1980_2019, color = "Predicted Inflation, 1982- training data")) +
  geom_line(aes(y = predicted_1970_2019, color = "Predicted Inflation, 1970- training data")) +
  labs(title = "Inflation Fell Against a Persistent Phillips Curve",
  subtitle="Actual vs. predicted PCE core inflation", y = "Inflation Rate", x = "Date",
       caption="Cleveland Fed 5-year expected inflation used for expectations for 1982-; FRB/US data for 1970-. u-star from CBO.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1982- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

ggsave("graphics/black_white_U.png", dpi="retina", width = 12, height=6.75, units = "in")

stargazer(model, model2, type = "html", column.labels = c("1992-2019","1971-2019"), dep.var.labels = c("$\\pi^*$"), header=FALSE)


pce_supply %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight, color=category)) + geom_point(aes(fill="skyblue"), alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "3-month Change July 2023 Minus 3-month Change December 2022, Quantity and Inflation, for ~130 Core PCE Item Categories",
       title="Deceleration is Driven by Expanded Supply",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = c(0.85,0.9)) +
  #scale_size_continuous(range = c(3, 10)) +
  facet_wrap(~category, scales = "free_y")
# Generate the table
options(knitr.kable.NA = '')
chart1 %>%
  kable(format = "latex", booktabs = TRUE, caption = "My Fancy Table", align = "c",
        col.names = c('', 'Location', 'Change', 'Decline', 'Location', 'Change', 'Decline','Location', 'Change', 'Decline')) %>%
  kable_styling(full_width = TRUE, latex_options = "scale_down") %>%
  add_header_above(header = c(" " = 1, "Core Items" = 3, "Goods" = 3, "Services" = 3))


cyclical_pce %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight)) + geom_point(alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "3-month Change June 2023 Minus 3-month Change June 2022, Quantity and Inflation, for ~130 Core PCE Item Categories",
       title="Deceleration is Driven by Expanded Supply",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = "bottom")


chart2 %>%
  kable(format = "latex", booktabs = TRUE, caption = "My Fancy Table 2 - Cyclical Categories", align = "c",
        col.names = c('', 'Location', 'Change', 'Decline')) %>%
  kable_styling(full_width = TRUE, latex_options = "scale_down")

