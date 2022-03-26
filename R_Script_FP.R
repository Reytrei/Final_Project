library("dplyr")
library("tidyr")
library("tidyverse")
library("ggplot2")


#Import Data
flight_94 <- read.csv("2005.csv",)
flight_95 <- read.csv("2006.csv")
planes_data <- read.csv("plane-data.csv")

#Join data
flights <- rbind(flight_94,flight_95)

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
  geom_histogram(binwidth = 100, stat = 'identity',)
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
flights_planes <- flights_planes %>% select(filter(Cancelled == 0),flights_age_columns)

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
  arrange(desc(minutes_ratio))

gr_age <- ggplot(delays_age, aes(x= Age, y=minutes_ratio)) + 
  geom_histogram(stat = 'identity')
gr_age
