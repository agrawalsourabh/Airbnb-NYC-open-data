# Since 2008, guests and hosts have used Airbnb to expand on traveling possibilities 
# and present more unique, personalized way of experiencing the world.This dataset describes 
# the listing activity and metrics in NYC, NY for 2019.

# installing packages

# importing libraries
library(ggplot2)
library(e1071)
library(scales)
library(dplyr)

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
  
