

library(tidyverse)
library(tidymodels)
library(ggplot2)
library(skimr)

data= read.csv("https://raw.githubusercontent.com/maria-pro/bco6008/main/train_airbnb.csv")
View(data)

skim(data)
sum(is.na(data))
data = na.omit(data)
sum(is.na(data))

pairs(data[,c(10,11,12,14,15,16)])

set.seed(123)
library(caTools)
spl = sample.split(Y = data$price, SplitRatio = 0.75)
#subsetting into Train data
train = data[spl,]
#subsetting into Test data
test = data[!spl,]
dim(train)
dim(test)

ggplot(data,aes(minimum_nights,price))+geom_point()+geom_smooth()
ggplot(data,aes(number_of_reviews,price))+geom_point()+geom_smooth()
ggplot(data,aes(reviews_per_month,price))+geom_point()+geom_smooth()

x <- table(data$room_type)
barplot(x)

y <- table(data$neighbourhood_group)
barplot(y)

z <- table(data$neighbourhood)
barplot(z)

ggplot(data = data, aes(x = price)) + geom_histogram()

stars_submeans <-data %>%
  group_by(neighbourhood_group) %>%
  summarise(mean_price=mean(price, na.rm=TRUE),
            median_price = median(price))
stars_submeans

p<-data %>%
  mutate(neighbourhood_group = fct_reorder(neighbourhood_group, price)) %>%
  ggplot( aes(x=neighbourhood_group, y=price)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
p
# From here we can see that the price for Manhattan is huge followed by Brooklyn, Queens.



q<-data %>%
  mutate(neighbourhood=fct_lump(neighbourhood, 40), neighbourhood=fct_reorder(neighbourhood, price))%>%
  ggplot( aes(x=neighbourhood, y=price)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
q
 
# Now the price is maximum for other for top 40 followed by Williamsburg, Bedford-Stuyvesant.

r<-data %>%
  mutate(room_type=fct_lump(room_type, 40), room_type=fct_reorder(room_type, price))%>%
  ggplot( aes(x=room_type, y=price)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
r
# The maximum price is for Entire home/apt followed by Private room then Shared room.


ggplot(data,aes(reviews_per_month,price))+geom_point()+geom_smooth(method="lm")
# The price is varying from 0 - 10000, and the reviews_per_month are varying from 0- 20 mostly.

ggplot(data,aes(calculated_host_listings_count,price))+geom_point()+geom_smooth(method="lm")
# The calculated_host_listings is not a siginificant predictor of price


ggplot(data,aes(availability_365,price))+geom_point()+geom_smooth(method="lm")
# The price varies with availability_365.

library(ggmap)
bbox <- c(left = -74.24285, bottom = 40.50641, right = -73.71690, top = 40.91306)
nyc_map <- get_stamenmap(bbox, zoom = 11)
aggregated_lat_lon <- train %>%
  group_by(latitude = round(latitude, 2),
           longitude = round(longitude, 2)) %>%
  summarize(price = mean(price),
            n = n()) %>%
  filter(n >= 5)


ggmap(nyc_map) +
  geom_point(aes(longitude, latitude, size = n, color = exp(price) - 1),
             data = aggregated_lat_lon) +
  scale_color_gradient2(low = "blue", high = "red", midpoint = 2,
                        trans = "log10", labels = dollar) +
  scale_size_continuous(range = c(.5, 4)) +
  labs(color = "Price",
       size = "# of listings")


#recipe_xg<- train %>%
#target= train$price
response = c('room_type','number_of_reviews','latitude',
               'longitude','neighbourhood_group','reviews_per_month',
               'calculated_host_listings_count','availability_365','last_review')
model2=lm(price~ room_type+number_of_reviews+latitude+longitude+
            neighbourhood_group+reviews_per_month+calculated_host_listings_count +
            availability_365+last_review, data=train)
summary(model2)

model3=lm(price~ room_type+
          latitude+
          longitude+
          neighbourhood_group+
          neighbourhood +
          host_id, data=train)
summary(model3)

