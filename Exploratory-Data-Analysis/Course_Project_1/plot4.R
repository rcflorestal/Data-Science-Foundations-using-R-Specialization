##-------------------Peer Graded Assignment Course Project 1------------------##
##                                                                            ##
##                                  Plot 4                                    ##
##                                                                            ##
##  This script is part of the Getting and Cleaning Data Module of the Data   ##
##  Science Specialization, offered by Johns Hopkins University through       ##
##  Coursera platform.                                                        ##
##                                                                            ##
##----------------------------------------------------------------------------##
#
## Load packages
library(dplyr)
library(tidyr)

## Set the work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Exploratory-Data-Analysis/Course_Project_1")

## Loading the data ##
dataSet <- read.table(file = "./data/household_power_consumption.txt",
                      header = TRUE,
                      sep = ";",
                      na.strings = "?")

## Read the dataset
head(dataSet)
tail(dataSet)

## Get more information about the current dataset
str(dataSet)


## Set a Subset using data from the dates 2007-02-01 and 2007-02-02, and convert
## the Date and Time character variables to date and time formats, respectively.
#
df <- dataSet %>%
        mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
               Time = as.POSIXct(strptime(paste(Date, Time), 
                                          "%Y-%m-%d %H:%M:%S"),
                                 tz = "Europe/Paris")) %>%
        filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>%
        drop_na()

## Read the new data frame
as_tibble(df)

## Open the graphics devices to save the plot
png("plot4.png", width = 480, height = 480)

## Set graphs parameters
par(mfrow = c(2, 2))

## Set plot 1-4 ##

plot(df$Global_active_power ~ df$Time, data = df, type = "l",
     ylab = "Global Active Power", xlab = "")

## Set plot 2-4 ##
plot(df$Voltage ~ df$Time, data = df, type = "l",
     ylab = "Voltage", xlab = "datetime")

## Set plot 3-4 ##
plot(df$Sub_metering_1 ~ df$Time,   ## Plot line Energy sub metering 1
     type = "l",
     xlab = "",
     ylab = "Energy sub metering")

lines(df$Sub_metering_2 ~ df$Time,  ## Plot line Energy sub metering 2
      type = "l",
      col = "red")

lines(df$Sub_metering_3 ~ df$Time,  ## Plot line Energy sub metering 3
      type = "l",
      col = "blue")

legend("topright", lty = 1, bty = "n",
       legend = c("Sub_metering_1",
                  "Sub_metering_2",
                  "Sub_metering_3"),
       col = c("black", "red", "blue"))

## Set plot 4-4 ##
plot(df$Global_reactive_power ~ df$Time, 
     data = df, 
     type = "l",
     lwd = 2.5,
     cex = 0.7,
     ylab = "Global_reactive_power", 
     xlab = "datetime")

## Close the graphics devices
dev.off()
