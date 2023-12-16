
x_values <- seq(0, 10, by = 0.1)
supply <- x_values
demand <- 10 - x_values
lower_demand <- 7 - x_values
higher_supply <- x_values - 3

# Data for arrows
arrow_data <- data.frame(
  x_start = c(3, 7),
  x_end = c(1, 9),
  y_start = c(7, 7),
  y_end = c(6, 6),
  category = c("Lower Demand", "Increased Supply")
)
arrow_data1 <- arrow_data[1,]
arrow_data_2 <- arrow_data[2,]




change <- lower_demand
data1 <- data.frame(x_values, supply, demand, change) %>% mutate(category = "Lower Demand")
change <- higher_supply
data2 <- data.frame(x_values, supply, demand, change) %>% mutate(category = "Increased Supply")

# Combine the data
data <- rbind(data1, data2)

# Set the category as a factor with specific levels
data$category <- factor(data$category, levels = c("Lower Demand", "Increased Supply"))

# Data for arrows
arrow_data <- data.frame(
  x_start = c(3, 7),
  x_end = c(1, 9),
  y_start = c(7, 7),
  y_end = c(6, 6),
  category = c("Lower Demand", "Increased Supply")
)
arrow_data$category <- factor(arrow_data$category, levels = c("Lower Demand", "Increased Supply"))

arrow_data2 <- data.frame(
  x_start = c(7, 3),
  x_end = c(5, 5),
  y_start = c(3, 3),
  y_end = c(2, 2),
  category = c("Lower Demand", "Increased Supply")
)
arrow_data2$category <- factor(arrow_data2$category, levels = c("Lower Demand", "Increased Supply"))

circles <- data.frame(
  x_values = c(3.5, 6.5),
  y_values = c(3.5, 3.5),
  category = c("Lower Demand", "Increased Supply")
)
circles$category <- factor(circles$category, levels = c("Lower Demand", "Increased Supply"))


annotate_corners <- data.frame(
  x_values = c(1, 9),
  y_values = c(3.5, 3.5),
  category = c("Lower Demand", "Increased Supply"),
  text_box = c("Lower\nDemand", "Increased\nSupply")
)
annotate_corners$category <- factor(annotate_corners$category, levels = c("Lower Demand", "Increased Supply"))

# Create the plot
slide4 <- ggplot(data1, aes(x = x_values)) +
  geom_line(aes(y = supply), color = "black", size = 1) +
  geom_line(aes(y = demand), color = "black", size = 1) +
  geom_line(aes(y = change), color = "deeppink4", linetype = "dotted", size = 1) +
  geom_line(data=data2, aes(y = change), color = "deeppink4", linetype = "dotted", size = 1) +
  geom_segment(data = arrow_data1, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               size = 0.5, color = "black") +
  geom_segment(data = arrow_data_2, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               size = 0.5, color = "black") +
  ylim(0, 10) +
  labs(x = "Quantity",
       subtitle = "Price",
       y="") +
  geom_point(data=circles, aes(x_values, y_values, fill="skyblue"), shape = 21, color = "black", stroke = 1.5, size=6, show.legend=FALSE) +
  geom_point(aes(x=5,y=5, fill="skyblue"), shape = 21, color = "black", stroke = 1.5, size=6, show.legend=FALSE) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(
    strip.background = element_blank(),  # Remove facet title box
    strip.text = element_blank(),
    plot.title.position = "plot") +
  geom_text(data=annotate_corners, aes(x_values, y_values, label = text_box), size=12) +
  theme(axis.text = element_blank())


slide4 <- slide4 +  theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)