remove_outliers <- function(x, multiplier = 1.5) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - multiplier * IQR
  upper_bound <- Q3 + multiplier * IQR
  x[(x < lower_bound) | (x > upper_bound)] <- NA
  return(x)
}

pce_supply <- read_csv("slide_code/pce_supply.csv")

slide5 <- pce_supply %>%
  mutate(QuantityFinal = remove_outliers(QuantityFinal, 4)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight, color=category)) + geom_point(aes(fill="skyblue"), alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(text = element_text(size = 25)) +
  labs(y = "",
       x = "Quantity",
       subtitle="Price") +
  facet_wrap(~category) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +  theme(strip.background = element_blank())


slide5 <- slide5 +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)