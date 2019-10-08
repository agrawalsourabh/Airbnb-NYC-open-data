# Since 2008, guests and hosts have used Airbnb to expand on traveling possibilities 
# and present more unique, personalized way of experiencing the world.This dataset describes 
# the listing activity and metrics in NYC, NY for 2019.

# installing packages

# importing libraries
library(ggplot2)
library(e1071)
library(scales)
library(dplyr)
library(tidyr)

# importing dataset
our.data = read.csv("input/AB_NYC_2019.csv", stringsAsFactors = F)

# ----  DATA PREPROCESSING  ------------------------
dim(our.data)
str(our.data)

# Missing Values 
missing_df = get_missing_data_df(our.data)
missing_df

# only reviews_per_month variable has missing values
# we'll replace all NA's with 0

our.data$reviews_per_month = ifelse(is.na(our.data$reviews_per_month), 0, 
                                    our.data$reviews_per_month)

# Still it might be possible that some variables contains empty fields, which we'll handle
# in our exploratory analysis

# ----  EXPLORATORY ANALYSIS  ------------------------

# Dropping 'Id' and 'host_name' from our dataset as they are insignificant
our.data = our.data[, -c(1, 3)]
head(our.data, 4)

# Seperate characters column and integer column for analysis
our.data.chars = Filter(is.character, our.data)
our.data.num = Filter(is.numeric, our.data)

# Let's explore one by one each numeric variables
colnames(our.data.num)

# price
summary(our.data.num$price)

# NOTE: We observe that price is highly right skewed, that means a large amount of 
# property price is less than $500

# Minimum Nights
unique(our.data.num$minimum_nights)

# Neighbourhood_group and price
our.data$neighbourhood_group = as.factor(our.data$neighbourhood_group)

price_per_neighbourhood_group = our.data %>% 
  group_by(neighbourhood_group) %>%
  summarise(median_price = median(price))

# /*
#   Workspace saved
#   */
  
# Note: Property price at Manhattan is highest among them and Bronx has the lowest.

# room_type
unique(our.data$room_type)

# these are categorical values, so change it to factors
our.data$room_type = as.factor(our.data$room_type)

# Note: Number of entire home/apt and Private room is much larger than shared room in our data set

# room_type and price
room_type_price_df = our.data %>%
  group_by(room_type) %>%
  summarise(median_price = median(price))

# Note: Price of Entire home/apt is higher than that of Private Room or Shared room

neigh_room.df = our.data %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(count_room = n())

my_test = neigh_room.df %>%
  group_by(neighbourhood_group) %>%
  summarise(sum = sum(count_room))

percent_vec = ifelse(neigh_room.df$neighbourhood_group == 'Bronx', 
                     neigh_room.df$count_room/my_test$sum[1], percent_vec)
percent_vec = ifelse(neigh_room.df$neighbourhood_group == 'Brooklyn', 
                     neigh_room.df$count_room/my_test$sum[2], percent_vec)
percent_vec = ifelse(neigh_room.df$neighbourhood_group == 'Manhattan', 
                     neigh_room.df$count_room/my_test$sum[3], percent_vec)
percent_vec = ifelse(neigh_room.df$neighbourhood_group == 'Queens', 
                     neigh_room.df$count_room/my_test$sum[4], percent_vec)
percent_vec = ifelse(neigh_room.df$neighbourhood_group == 'Staten Island', 
                     neigh_room.df$count_room/my_test$sum[5], percent_vec)

neigh_room.df$percentage = round(percent_vec*100, 2)

# Note: From RoomType percentage Neighbourhood area, we found out that 
# Manhattan - large number of entire home peresent as compared to Private and shared
# Sim, Bronx and Queens - More number of private room present as compared to Entire home and shared
# Broooklyn and Staten Island - Have almost same number of Entire or Private property available

# Neighbourhood
unique(our.data$neighbourhood)

# longitude and latitude

# number of reviews
length(unique(our.data$number_of_reviews))

# minimum_nights group by room_type
min_night_room_type.df = our.data %>%
  group_by(room_type) %>%
  summarise(minimum_nights = median(minimum_nights))

# room_type and reviews_per_month
rt_rpm.df = our.data %>%
  group_by(room_type) %>%
  summarise(reviews_per_month = sum(reviews_per_month))

rt_rpm.df$percentage[1] = round(rt_rpm.df$reviews_per_month[1] / sum(rt_rpm.df$reviews_per_month) *100
                                , 2) 
rt_rpm.df$percentage[2] = round(rt_rpm.df$reviews_per_month[2] / sum(rt_rpm.df$reviews_per_month) *100
                                , 2) 
rt_rpm.df$percentage[3] = round(rt_rpm.df$reviews_per_month[3] / sum(rt_rpm.df$reviews_per_month) *100
                                , 2) 

# room_type and number_of_reviews
rt_total_reviews.df = our.data %>%
  group_by(room_type) %>%
  summarise(number_of_reviews = sum(number_of_reviews))

rt_total_reviews.df$percentage[1] = round(rt_total_reviews.df$number_of_reviews[1] / sum(rt_total_reviews.df$number_of_reviews) *100
                                , 2) 
rt_total_reviews.df$percentage[2] = round(rt_total_reviews.df$number_of_reviews[2] / sum(rt_total_reviews.df$number_of_reviews) *100
                                , 2) 
rt_total_reviews.df$percentage[3] = round(rt_total_reviews.df$number_of_reviews[3] / sum(rt_total_reviews.df$number_of_reviews) *100
                                , 2) 

# /*
#   Workspace saved
#   */