data <- read.csv('activity.csv')

names(data) <- c('steps','day','interval')

agg <- aggregate(x = data$steps,                # Specify data column
                by = list(data$day),           # Specify group indicator
                FUN = sum,                     # Specify function (i.e. sum)
                na.rm = TRUE)               # remove NAs

names(agg) <- c('day','daily_steps')

#head(agg)

# Create the histogram.
hist(agg$daily_steps,xlab = "Daily Steps",
     col = "yellow",border = "blue", main="Histogram of Daily Steps Data")

mean(agg$daily_steps, na.rm = TRUE)
median(agg$daily_steps, na.rm = TRUE)


agg2 <- aggregate(x = data$steps,                # Specify data column
                 by = list(data$interval),       # Specify group indicator
                 FUN = mean,                     # Specify function (i.e. sum)
                 na.rm = TRUE)                   # remove NAs
names(agg2) <- c('interval','average_steps')

library(ggplot2)

ggplot(data=agg2, aes(x=interval, y=average_steps)) +
    geom_line(col='blue') +
    labs(title = "Average Steps by Time Interval in the Day"
         , x = "Interval", y = "Average Steps in Period") 

agg2$interval[ which.max(agg2$average_steps) ]

col_count_NA <- colSums(is.na(data))
total_count_NA <- sum(col_count_NA)
total_count_NA

# Create a copy of the data frame called 'data'
# and save it as 'data2'
library(data.table)
adj_data <- copy(data)

# Replace all NA values for 'steps'
# with the mean for the 'interval' calculated
# by excluding all NA values

for (i in 1:dim(adj_data)[1]) {
    if(is.na(adj_data[i,1])) {
        adj_data[i,1] <- agg2[agg2$interval==adj_data[i,3],2]
    }
}

# Histogram of adjusted daily steps
# with missing data filled in with mean from
# the interval in the day excluding missing values

adj_agg <- aggregate(x = adj_data$steps,        # Specify data column
                    by = list(data$day),        # Specify group indicator
                    FUN = sum,                  # Specify function (i.e. sum)
                    na.rm = TRUE)               # remove NAs

names(adj_agg) <- c('day','daily_steps')

# Create the histogram.
hist(adj_agg$daily_steps,xlab = "Daily Steps",
     col = "green",border = "red", main="Adjusted Histogram of Daily Steps Data")

mean(adj_agg$daily_steps, na.rm = TRUE)
median(adj_agg$daily_steps, na.rm = TRUE)
max(adj_agg$daily_steps, na.rm = TRUE)
min(adj_agg$daily_steps, na.rm = TRUE)

weekdays(data$day)
weekdays(as.Date(data$day,format="%Y-%m-%d"))
length(data$day)

# Taking advantage of the fact that Saturday and Sunday 
# are the only days starting with S:
data$Weekend <- grepl("S.+",weekdays(as.Date(data$day,format="%Y-%m-%d")))
#agg$Weekend <- grepl("S.+",weekdays(as.Date(agg$day,format="%Y-%m-%d")))

#agg[ agg$Weekend == TRUE, 3] <- "weekend"
#agg[ agg$Weekend == FALSE, 3] <- "weekday"

temp_data <- data[ data$Weekend==FALSE,]
wkday_agg2 <- aggregate(x = temp_data$steps,                # Specify data column
                  by = list(temp_data$interval),       # Specify group indicator
                  FUN = mean,                     # Specify function (i.e. sum)
                  na.rm = TRUE)                   # remove NAs
names(wkday_agg2) <- c('interval','average_steps')

temp_data <- data[ data$Weekend==TRUE,]
wkend_agg2 <- aggregate(x = temp_data$steps,                # Specify data column
                        by = list(temp_data$interval),       # Specify group indicator
                        FUN = mean,                     # Specify function (i.e. sum)
                        na.rm = TRUE)                   # remove NAs
names(wkend_agg2) <- c('interval','average_steps')



par(mfcol = c(2, 1))

# TOP
plot(wkend_agg2$interval, wkend_agg2$average_steps, 
     type = "l",
     col = "red",
     xlab = "Interval during the day", 
     ylab = "Average step count",
     main = "Weekend") 

# BOTTOM
plot(wkday_agg2$interval, wkday_agg2$average_steps, 
     type = "l",
     col = "blue",
     xlab = "Interval during the day", 
     ylab = "Average step count", 
     main = "Weekday") 
