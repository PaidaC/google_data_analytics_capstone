### Divvy_Exercise_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished': Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: "In what ways do members and casual riders use Divvy bikes differently?"

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)    #helps visualize data
library(dplyr)      #helps with data manipulation
getwd() #display working directory
setwd("C:/Users/USER/Documents/Cyclistic trip data/Analysis files R")
getwd() #check if wd change was successful

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files)
june_tripsdata <- read_csv("202206-divvy-tripdata.csv")
july_tripsdata <- read_csv("202207-divvy-tripdata.csv")
august_tripsdata <- read_csv("202208-divvy-tripdata.csv")
september_tripsdata <- read_csv("202209-divvy-publictripdata.csv")
october_tripsdata <- read_csv("202210-divvy-tripdata.csv")
november_tripsdata <- read_csv("202211-divvy-tripdata.csv")
december_tripsdata <- read_csv("202212-divvy-tripdata.csv")
january_tripsdata <- read_csv("202301-divvy-tripdata.csv")
february_tripsdata <- read_csv("202302-divvy-tripdata.csv")
march_tripsdata <- read_csv("202303-divvy-tripdata.csv")
april_tripsdata <- read_csv("202304-divvy-tripdata.csv")
may_tripsdata <- read_csv("202305-divvy-tripdata.csv")


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(june_tripsdata)
colnames(july_tripsdata)
colnames(august_tripsdata)
colnames(september_tripsdata)
colnames(october_tripsdata)
colnames(november_tripsdata)
colnames(december_tripsdata)
colnames(january_tripsdata)
colnames(february_tripsdata)
colnames(march_tripsdata)
colnames(april_tripsdata)
colnames(may_tripsdata)

# Inspect the dataframes and look for incongruencies
str(june_tripsdata)
str(july_tripsdata)
str(august_tripsdata)
str(september_tripsdata)
str(october_tripsdata)
str(november_tripsdata)
str(december_tripsdata)
str(january_tripsdata)
str(february_tripsdata)
str(march_tripsdata)
str(april_tripsdata)
str(may_tripsdata)

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(may_tripsdata, june_tripsdata, july_tripsdata,
                       august_tripsdata, september_tripsdata, october_tripsdata,
                       november_tripsdata, december_tripsdata, january_tripsdata
                       , february_tripsdata, march_tripsdata, april_tripsdata,)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

#Rename column member_casual to customer_type
all_trips <- rename(all_trips, "customer_type" = "member_casual")

# There are a few problems we will need to fix:
# (1) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length, add "ride_length" to the entire dataframe.
# (4) There are some rides where ride_length shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# Add columns that list the date, month, day, year and start time of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- months(all_trips$date, abbreviate = FALSE)#gives month
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- wday(all_trips$started_at, label = TRUE) #gives the labeled day ie Sunday instead of 1
all_trips$start_time <- format(all_trips$started_at, format = "%H:%M")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
# method used to give ride length in the format HH:MM:ss
all_trips$ride_length <- (difftime(all_trips$ended_at,all_trips$started_at))

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#check for missing data
NROW(all_trips$ride_id[is.na(all_trips$ride_id)])
NROW(all_trips$rideable_type[is.na(all_trips$rideable_type)])
NROW(all_trips$started_at[is.na(all_trips$started_at)])
NROW(all_trips$ended_at[is.na(all_trips$ended_at)])
NROW(all_trips$customer_type[is.na(all_trips$customer_type)])
NROW(all_trips$start_station_name[is.na(all_trips$started_at)])
NROW(all_trips$end_station_name[is.na(all_trips$started_at)])
NROW(all_trips$start_station_id[is.na(all_trips$started_at)])
NROW(all_trips$end_station_id[is.na(all_trips$started_at)])
NROW(all_trips$start_lat[is.na(all_trips$started_at)])
NROW(all_trips$start_lng[is.na(all_trips$started_at)])
NROW(all_trips$end_lat[is.na(all_trips$started_at)])
NROW(all_trips$end_lng[is.na(all_trips$started_at)])
NROW(all_trips$date[is.na(all_trips$date)])
NROW(all_trips$month[is.na(all_trips$month)])
NROW(all_trips$day[is.na(all_trips$day)])
NROW(all_trips$year[is.na(all_trips$year)])
NROW(all_trips$day_of_week[is.na(all_trips$day_of_week)])
NROW(all_trips$start_time[is.na(all_trips$start_time)])
NROW(all_trips$ride_length[is.na(all_trips$ride_length)])


# check for duplicates
NROW(all_trips[duplicated(all_trips$ride_id),]) # no duplicates found

# remove bad data
# rides less than 0 are erroneous and rides less than 60 could indicate false starts
all_trips_v2 <- all_trips[!(all_trips$ride_length < 60),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride
sd(all_trips_v2$ride_length) #standard deviation 

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = sd)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(customer_type, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(customer_type, weekday)								# sorts

#average ride length on weekends
all_trips_v2[(all_trips_v2$day_of_week == "Sun"|all_trips_v2$day_of_week == "Sat"),] %>%
  group_by(customer_type) %>%
  summarise(mean(ride_length))

#total number of rides
TotalRides <- all_trips_v2 %>%
  group_by(customer_type) %>%
  summarise(n())

# casual rides occurring on sat
SaturdayRides <- all_trips_v2[(all_trips_v2$day_of_week == "Sat"),] %>%
  group_by(customer_type) %>%
  summarise(n())


# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = customer_type)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge")

# Creating aggregated tables to be used to create visualizations in tableau

# table showing frequency and average duration of rides throughout the week
rides_per_weekday <- all_trips_v2 %>%
  group_by(day_of_week, customer_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length))

write.csv(rides_per_weekday, file = 'rides_per_weekday.csv')

#table showing frequency and duration of rides on the different bike types
rides_per_biketype <- all_trips_v2 %>%
  group_by(rideable_type, customer_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length))

write.csv(rides_per_biketype, file = 'rides_per_biketype.csv')

#table showing frequency and duration of rides per month
rides_per_month <- all_trips_v2 %>%
  group_by(month, customer_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length))

#sd
aggregate(rides_per_month$average_duration ~ rides_per_month$customer_type, FUN = sd)
aggregate(rides_per_month$number_of_rides ~ rides_per_month$customer_type, FUN = sd)


# table showing frequency and duration of rides each day
rides_per_date <- all_trips_v2 %>%
  group_by(date, customer_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length))

write.csv(rides_per_date, file = 'rides_per_date.csv')

#table showing frequency and duration of rides per days of month
# only done for the first 28 days to cater for february and 30 day months
rides_per_day <- all_trips_v2[(all_trips_v2$day < 29),]  %>%
  group_by(day, customer_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length))

write.csv(rides_per_day, file = 'rides_per_day.csv')

# sd 
aggregate(rides_per_day$average_duration ~ rides_per_day$customer_type, FUN = sd)


# table showing frequency and duration of rides per start of start
rides_per_hour <- all_trips_v2 %>%
  group_by(start_time, customer_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length))

write.csv(rides_per_hour, file = 'rides_per_hour.csv')

#sd
aggregate(rides_per_hour$number_of_rides ~ rides_per_hour$customer_type, FUN = sd)
aggregate(rides_per_hour$average_duration ~ rides_per_hour$customer_type, FUN = sd)

write.csv(all_trips_v2, file = 'all_trips_data')
