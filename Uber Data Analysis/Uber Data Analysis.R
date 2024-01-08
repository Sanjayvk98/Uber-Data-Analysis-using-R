library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) # metapackage of all tidyverse packages
library(DT)
library(scales)
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors
apr <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-apr14.csv")
may <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-may14.csv")
june <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-jun14.csv")
july <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-jul14.csv")
aug <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-aug14.csv")
sept <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-sep14.csv")

# Combine the data together 
data <- rbind(apr, may, june, july, aug, sept)
cat("The dimensions of the data are:", dim(data))
head(data)

data<-na.omit(data)
dim(data)
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date<-factor(date(data$Date.Time))
data$Date.Time <- ymd_hms(data$Date.Time)
# Create individual columns for month day and year
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))
data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))
data$week = factor(chron::is.weekend(data$Date.Time))
data$week<-factor(ifelse(data$week == "TRUE", "Weekend", "Weekday"))

data<-data%>%drop_na()
head(data)
#df$day_type <- ifelse(df$week == "TRUE", "Weekend", "Weekday")
#data$week<-factor(ifelse(data$week == "TRUE", "Weekend", "Weekday"))
head(data)

dim(data)
hourly_data <- data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

# Shos data in a searchable js table
datatable(hourly_data)
ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="black") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
monthly_data <- data %>% 
  group_by(month) %>% 
  dplyr::summarize(Total = n())

ggplot(monthly_data, aes(month, Total)) + 
  geom_bar(stat="identity", 
           fill="aliceblue", 
           color="black") + 
  ggtitle("Trips Every Month", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
daily_data <- data %>% 
  group_by(day) %>% 
  dplyr::summarize(Total = n())
ggplot(daily_data, aes(day, Total)) + 
  geom_bar(stat="identity", 
           fill="plum", 
           color="lightgreen") + 
  ggtitle("Trips Every Day", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
daily_week_data <- data %>% 
  group_by(dayofweek) %>% 
  dplyr::summarize(Total = n())
ggplot(daily_week_data, aes(dayofweek, Total)) + 
  geom_bar(stat="identity", 
           fill="salmon", 
           color="wheat") + 
  ggtitle("Trips Every Week", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
daily_weekend_data <- data %>% 
  group_by(week) %>% 
  dplyr::summarize(Total = n())
ggplot(daily_weekend_data, aes(week, Total)) + 
  geom_bar(stat="identity", 
           fill="lightcyan", 
           color="navy") + 
  ggtitle("Trips Every Week", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

#------------------------------------------------------------------------------------
month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)
month_week_data <- data %>% group_by(month, dayofweek) %>%  dplyr::summarize(Total = n())

ggplot(month_week_data, aes(month, Total, fill=dayofweek)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Week and Month") + 
  scale_y_continuous(labels = comma)

day_week_data <- data %>% group_by(day, month) %>%  dplyr::summarize(Total = n())

ggplot(day_week_data, aes(day, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma)

month_weekend_data <- data %>% group_by(month, week) %>%  dplyr::summarize(Total = n())

ggplot(month_weekend_data, aes(month, Total, fill=week)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by month and Weekdays/Weekends") + 
  scale_y_continuous(labels = comma)

dayweek_week_data <- data %>% group_by(dayofweek, week) %>%  dplyr::summarize(Total = n())

ggplot(dayweek_week_data, aes(dayofweek, Total, fill=week)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Day and Weekdays") + 
  scale_y_continuous(labels = comma)

#-------------------------------------------------------------------------------
# Collect data by day of the week and month

day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
day_month_data
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

dayweek_month_data <- data %>% group_by(week, month) %>% dplyr::summarize(Trips = n())
dayweek_month_data
ggplot(dayweek_month_data, aes(week, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trip by Weekdays and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

days_month_data <- data %>% group_by(day, month) %>% dplyr::summarize(Trips = n())
days_month_data
ggplot(days_month_data, aes(day, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trip by Weekdays and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)
#--------------------------------------------------------------------------------
month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())

month_data
day_hour_data <- data %>% group_by(day, hour) %>% dplyr::summarize(Total = n())
datatable(day_hour_data)
ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Hour and Day")

ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "black") + 
  ggtitle("Heat Map by Week and Month")

ggplot(month_hour_data, aes(hour, month, fill = Total)) + 
  geom_tile(color = "darkgrey") + 
  ggtitle("Heat Map by Hour and Month")

month_day_data <- data %>% group_by(month, day) %>% dplyr::summarize(Trips = n())
month_day_data
ggplot(month_day_data, aes(day, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

#---------------------------------------------------------------------------------------------
min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

#ggplot(apr, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

mean_d<-mean(month_data$Total)
mean_d
    # Perform one-sample t-test
t_test_result <- t.test(month_data$Total, mu = 65000)  # mu is the population mean under the null hypothesis
  
  # Display results
print(t_test_result)  
#--------------------------------------------------------------------------------------------------------------------
observed_data <- matrix(dayweek_month_data$Trips, nrow = 4,ncol=3)

# Perform chi-square test
chi_square_result <- chisq.test(observed_data)

# Display results
print(chi_square_result)
#----------------------------------------------------------------------------------------------------------------------------
summary(data)

cat("End")
