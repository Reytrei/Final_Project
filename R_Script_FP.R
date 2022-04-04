library("dplyr")
library("tidyr")
library("tidyverse")
library("ggplot2")
library('vioplot')
library('igraph')

#Import Data
planes_data <- read.csv("plane-data.csv")
airport_data <- read.csv("airports.csv")
flights <- read.csv("sampled_data.csv")


#Vector to select columns
delay_columns <- c("Year","Month","DayofMonth","DayOfWeek","CRSDepTime","ArrDelay","DepDelay")
#Eliminate cancelled flights and select relevant columns 
delays_df <- select(filter(flights, Cancelled == 0),delay_columns)
#Check Arrivals NAs
nas <- filter(flights, is.na(ArrDelay) == TRUE & Cancelled == 0)

#Drop NAs
delays_df <- delays_df %>% drop_na()

#Create colum that has 1 for delayed 0 for on time
delays_df <- delays_df %>% 
  mutate(delayed = case_when(delays_df$ArrDelay == 0 ~ 0, TRUE ~ 1))

#Calculate delays for day of week
delays_dayofweek <- delays_df %>% 
  group_by(DayOfWeek) %>% 
  summarise(delayes = sum(delayed), total_flight = n(), flight_ratio= delayes*100/total_flight, minutes_delayed = sum(abs(ArrDelay)),minutes_ratio = minutes_delayed/total_flight) %>%
  arrange(desc(minutes_ratio))

#Delays for day
delays_month <- delays_df %>% 
  group_by(Month) %>% 
  summarise(delayes = sum(delayed), total_flight = n(), flight_ratio= delayes*100/total_flight, minutes_delayed = sum(abs(ArrDelay)),minutes_ratio = minutes_delayed/total_flight) %>%
  arrange(desc(minutes_ratio))

#Delays for time
delays_time <- delays_df %>% 
  group_by(CRSDepTime) %>% 
  summarise(delayes = sum(delayed), total_flight = n(), flight_ratio= delayes*100/total_flight, minutes_delayed = sum(abs(ArrDelay)),minutes_ratio = minutes_delayed/total_flight) %>%
  filter(CRSDepTime > 500) %>%
  arrange(desc(minutes_ratio))

#Graphics
gr_dayofweek <- ggplot(delays_dayofweek, aes(x= DayOfWeek, y= minutes_ratio, colour= DayOfWeek, fill = DayOfWeek)) + 
  geom_bar(stat = 'identity') +
  labs(title = "Delay per day of month", x = "Day of Week", y = "Minutes ratio")
  
gr_dayofweek

gr_month <- ggplot(delays_month, aes(x= Month, y= minutes_ratio, colour= Month, fill = Month)) + 
  geom_bar(stat = 'identity')

gr_month

gr_time <- ggplot(delays_time, aes(x= CRSDepTime, y= minutes_ratio)) + 
  geom_histogram(binwidth = 100, stat = 'identity')
gr_time 

gr_distribution <- ggplot(delays_df, aes(x=CRSDepTime)) + 
  geom_histogram()

gr_distribution

#Setting data for second question. Joining info from flights with plane data
flights_planes <- flights %>%  
  left_join(planes_data, by= c("TailNum" = "tailnum"))
 
#Dropping missing values for plane year of manufacturing
flights_planes  <- flights_planes %>% 
  drop_na("year")
#Select relevant columns
flights_age_columns = c("Year","Month","DayofMonth","ArrDelay","year")
flights_planes <- select(filter(flights_planes,Cancelled == 0),flights_age_columns)

#Correct column year format to numeric
flights_planes[,5] <- sapply(flights_planes[,5], as.numeric)
#drop NAs generated in change of character to numeric and ArrDelay NAs
flights_planes <- drop_na(flights_planes)
#Eliminating outliers
flights_planes <- flights_planes %>% filter( year > 0 & year<2007)
#Calculating Age of planes
flights_planes <- flights_planes %>% mutate(Age = Year - year)

delays_age <- flights_planes %>% 
  group_by(Age) %>%
  summarise(total_flight = n(), minutes_delayed = sum(abs(ArrDelay)),minutes_ratio = minutes_delayed/total_flight) %>%
  filter(total_flight>20) %>%
  arrange(desc(minutes_ratio))

gr_age <- ggplot(delays_age, aes(x= Age, y=minutes_ratio)) + 
  geom_histogram(stat = 'identity')
gr_age

#Setting data for question 3
flights_people <- flights %>%
  left_join(airport_data, by = c("Origin" = "iata"))

flights_people <- rename(flights_people,  "Origin_city" = "city")
flights_people <- flights_people %>% left_join(airport_data, by = c("Dest" = "iata"))
flights_people <- rename(flights_people, "Destination_city" = 'city')
flights_people_columns <- c("Year","Month", "Origin_city", "Destination_city")
flights_people <- select(filter(flights_people,Cancelled == 0),flights_people_columns)
colSums(is.na(flights_people))
flights_people <- drop_na(flights_people)

flights_people_data <- flights_people %>%
  group_by(Origin_city,Destination_city) %>% 
  summarise( Total_flights = n(), Year_2005 = sum(Year == 2005), Year_2006 = sum(Year== 2006), Difference = Year_2006 - Year_2005) %>%
  arrange(desc(Total_flights))

flights_people_2005 <- filter(flights_people, Year == 2005)
flights_people_2006 <- filter(flights_people, Year == 2006)

flights_people_2005_Origin <- flights_people_2005 %>% 
  group_by(Origin_city) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))%>%
  top_n(10)

flights_people_2005 <- flights_people_2005[,c(3,4,1,2)]
airport_data_cities <- airport_data[,c(3,1,2,4,5,6,7)]
airport_data_cities <- airport_data_cities %>% distinct(city) %>% drop_na()
airport_data_cities_short <-  airport_data_cities %>% filter(city %in% flights_people_2005_filtered$Origin_city | city %in% flights_people_2005_filtered$Destination_city)

network_2005 <- graph_from_data_frame(d= flights_people_2005_filtered, vertices = airport_data_cities_short, directed = T)
network_2005 <- simplify(network_2005, remove.multiple = F, remove.loops = T)
plot(network_2005, edge.arrow.size = 1)
V(network_2005)$size <- degree(network_2005, mode = "all")
E(network_2005)$width <- E(network_2005)$weight/6
network_2005_sp <- delete_edges(network_2005, V(network_2005)$size < 20 )

flights_people_2005$linked <- paste(flights_people_2005$Origin_city, flights_people_2005$Destination_city)
flights_people_2005_filter <- flights_people_2005 %>% 
  group_by(linked) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  top_n(20)
flights_people_2005_filter <- flights_people_2005_filter$linked
flights_people_2005_filtered <- flights_people_2005 %>%
  filter(linked %in% flights_people_2005_filter)



