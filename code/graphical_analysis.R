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

ggplot(data = price_per_neighbourhood_group, mapping = aes(x = neighbourhood_group, 
                                                           y = median_price)) + 
  geom_bar(stat = 'identity', fill = '#5188e0', col = '#095ee6', alpha = 0.8) +
  ggtitle("Median price per neighbourhood group") +
  xlab("Neighbourhood group") +
  ylab("Median Price") +
  scale_y_continuous(labels = dollar) +
  geom_label(mapping = aes(label = paste('$', median_price)))

ggsave("plots/PriceAndNeighbourhoodGroup.png")

# room_type
# plot a bar graph for room_type

ggplot(data = our.data, mapping = aes(x = room_type)) +
  geom_bar(fill = '#66edd4', col = '#07876f', alpha = 0.8) +
  ggtitle("Room Type Analysis") +
  xlab("Room Type") +
  ylab("Count") +
  geom_label(mapping = aes(label = ..count..), stat = 'count')

ggsave("plots/RoomType_Bar.png")

# room_type and price
ggplot(data = room_type_price_df, mapping = aes(x = room_type, y = median_price)) +
  geom_bar(stat = 'identity', fill='#8eba47', col='#2a4006') +
  ggtitle("Room Type and their price (median)") +
  xlab("Room Type") +
  ylab("Median Price") +
  scale_y_continuous(labels = dollar) + 
  geom_label(mapping = aes(label = paste("$", median_price)))

ggsave("plots/RoomType_Price.png")

# which type of room is popular in neighbourhood_group
ggplot(data = our.data, mapping = aes(x = neighbourhood_group, fill=room_type)) +
  geom_bar()


##

ggplot(neigh_room.df, aes(room_type, percentage)) + 
  geom_bar(stat = 'identity', fill='#f5ae62', col='#8a4f0f', alpha = 0.8, width = 0.5) + 
  facet_grid(~ neighbourhood_group) +
  geom_label(mapping = aes(label = paste(percentage, "%"))) +
  ggtitle("Percentage of room type per neighbourhood area") +
  xlab("Percentage") +
  ylab("Room Type")

ggsave("plots/RoomType_Perc_NG.png")


# plot scatter plot between longitude and latitude
ggplot(data = our.data, mapping = aes(x = latitude, y = longitude, col=price)) +
  geom_point() +
  scale_color_gradient(low = '#8af2a6', high = '#0b872c')

ggsave("plots/long_lat_scatter.png")


# number_of_reviews and minimum_nights with price
# ggplot(data = our.data, mapping = aes(x = minimum_nights, y = price, col=price))+
#   geom_point() +
#   scale_color_gradient(low = '#83aef2', high = '#1a4c9c')

# density graph for minimum nights
ggplot(data = our.data, mapping = aes(x = minimum_nights)) +
  geom_density()
skewness(our.data$minimum_nights)

# minimum_nights and room_type
ggplot(data = min_night_room_type.df, mapping = aes(x = room_type, y = minimum_nights, 
                                                    fill = minimum_nights)) +
  geom_bar(stat = 'identity', width = 0.8) +
  ggtitle("Room available for minimum nights") +
  xlab('Room Type') +
  ylab('Minimum Nights (Median)')+
  scale_fill_gradient(low = "#f7968f", high = "#bd6a64") +
  theme(legend.position = 'none')

ggsave("plots/MinimumNight_RoomType_bar.png")

# review_per_month based on room type
ggplot(data = rt_rpm.df, mapping = aes(x = reorder(room_type, percentage), 
                                       y = percentage)) +
  geom_bar(stat = 'identity', col = '#228032', fill = '#77f28b', alpha = 0.8) +
  coord_flip() +
  ggtitle("Reviews per month based on room type") +
  xlab("Room Type") +
  ylab(" % Reviews per Month") +
  geom_label(mapping = aes(label = paste(percentage, "%")))

ggsave("plots/Permonth_Review_Perc_RoomType.png")


# Plot a pie chart 

ggplot(data = rt_rpm.df, aes(x = "", y = percentage, fill = room_type)) +
  geom_bar( stat = "identity", color = "white", fill = brewer.pal(n =3, name="Dark2")) +
  coord_polar("y", start = 0)+
  geom_text(aes(label = paste(percentage, "%")), color = "white", position = position_stack(vjust = 0.5))+
  theme_void() +
  ggtitle("Reviews per month based on room type")

ggsave("plots/Permonth_Review_Perc_RoomType_PieChart.png")


# total reviews based on room type
ggplot(data = rt_total_reviews.df, mapping = aes(x = reorder(room_type, percentage), 
                                       y = percentage)) +
  geom_bar(stat = 'identity', col = '#58aed6', fill = '#1e5a75', alpha = 0.8) +
  coord_flip() +
  ggtitle("Total number of reviews based on room type") +
  xlab("Room Type") +
  ylab(" % Reviews") +
  geom_label(mapping = aes(label = paste(percentage, "%")))

ggsave("plots/Review_Perc_RoomType.png")

# Plot donut chart for rt_total_review.df
ggplot(data = rt_total_reviews.df, mapping = aes(x = 2, y = percentage, fill=room_type)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = 3, name = "Dark2"), col="White") + 
  coord_polar(theta = "y") +
  geom_text(mapping = aes(label = paste(percentage, "%")), position = position_stack(vjust = 0.5), 
            color = "white") +
  theme_void() +
  xlim(0.5, 2.5) +
  ggtitle("Total number of reviews based on room type")
    
ggsave("plots/Review_Perc_RoomType_Donut.png")

# /*
#   Workspace saved
#   */