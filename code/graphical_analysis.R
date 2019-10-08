# Graphical analysis

# Numeric Variables
# Price - Create histograms and density plots

ggplot(data = our.data.num, mapping = aes(x = price)) +
  geom_density(fill = "#45d16a", col = "#054516", alpha = 0.8) +
  ggtitle("Price Density Graph")

ggsave("plots/price_density.png")
# skewness(our.data.num$price) = 19.11777


# Scatter plot between Price and minimum_nights
ggplot(data = our.data.num, mapping = aes(x = minimum_nights, y = price)) +
  geom_point()

# Neighbourhood_group and price
our.data$neighbourhood_group
our.data$price
ggplot(data = our.data, mapping = aes(x = neighbourhood_group, 
                                      y = median(price))) +
  geom_bar(stat = 'identity', col = "#ad1105", alpha = 0.5) + 
  ggtitle(" Bar Graph b/w Price and Neighbourhood Group") +
  xlab("Neighbourhood group") +
  ylab("Price") +
  scale_y_continuous(labels = dollar)