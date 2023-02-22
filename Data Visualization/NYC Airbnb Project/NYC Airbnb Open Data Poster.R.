install.packages('ggcorrplot')

library(data.table)
library(dplyr)
library(scales)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(ggExtra)
library(ggcorrplot)
library(tm)
library(RColorBrewer)

airbnb <- read.csv("AB_NYC_2019.csv", 
                   encoding="UTF-8", stringsAsFactors = F, na.strings = c("","NA"))


## Replacing NA values in reviews_per_month with 0 
airbnb <- na.omit(airbnb)
airbnb$reviews_per_month<-as.factor(airbnb$reviews_per_month)
summary(airbnb)
str(airbnb)


# left 1: neighbourhood_group #scales
airbnb %>% 
  ggplot(aes(neighbourhood_group)) + 
  geom_bar(fill = '#FF5A5F', colour = 'black') + 
  geom_text(aes(label = percent(..prop..), y = ..count.., group = 1), stat = 'count', vjust = -0.5) + 
  labs(x = 'Neighbourhood_Group')

# left 2: price
airbnb %>% 
  ggplot(aes(price)) + 
  geom_histogram(binwidth = 0.025, fill = '#FF5A5F', colour = 'black') + 
  geom_vline(xintercept = mean(airbnb$price), linetype = 'dashed', colour = 'red', size = 1) + 
  scale_x_log10(labels = dollar_format(prefix = '$')) 


# left 3: maps
ggplot(data=airbnb)+geom_point(aes(x=longitude, y=latitude, color=neighbourhood_group))


# right 1: room_type
airbnb %>% 
  ggplot(aes(room_type)) + 
  geom_bar(fill = '#FF5A5F', colour = 'black') + 
  geom_text(aes(label = percent(..prop..), y = ..count.., group = 1), stat = 'count', vjust = -0.5) + 
  labs(x = 'Room_Type')


# right 2: neighbourhood_group / room_type #gridextra
p1 <- airbnb %>% 
  ggplot(aes(neighbourhood_group, fill = room_type)) + 
  geom_bar(position = 'dodge', colour = 'black') + 
  labs(x = 'Neighbourhood_Group') + 
  guides(fill = F)

temp <- airbnb %>% 
  group_by(neighbourhood_group, room_type) %>% 
  count() %>% 
  group_by(neighbourhood_group) %>% 
  mutate(prop = round(n/sum(n), 3))

p2 <- airbnb %>% 
  ggplot(aes(neighbourhood_group)) + 
  geom_bar(aes(fill = room_type), colour = 'black', position = 'fill') + 
  geom_text(data = temp, aes(y = prop, label = percent(prop), group = room_type), stat = 'identity', position = position_stack(vjust = 0.5)) + 
  labs(x = 'Neighbourhood_Group', y = 'prop', fill = 'Room_Type')

grid.arrange(p1, p2, ncol = 2)

# right 3: neighbourhood_group / price / room_type
airbnb %>% 
  group_by(room_type, neighbourhood_group) %>% 
  summarise(median_price = median(price)) %>% 
  ggplot(aes(neighbourhood_group, median_price)) + 
  geom_bar(aes(fill = neighbourhood_group), stat = 'identity', colour = 'black') + 
  geom_text(aes(label = median_price), vjust = -0.5) + 
  guides(fill = F) + 
  labs(x = 'Neighbourhood Group', y = 'Median of Price') + 
  facet_wrap(~ room_type)

