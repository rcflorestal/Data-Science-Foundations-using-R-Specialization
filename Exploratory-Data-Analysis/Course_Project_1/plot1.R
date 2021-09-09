##-------------------Peer Graded Assignment Course Project 1------------------##
##                                                                            ##
##                                  Plot 1                                    ##
##                                                                            ##
##  This script is part of the Getting and Cleaning Data Module of the Data   ##
##  Science Specialization, offered by Johns Hopkins University through       ##
##  Coursera platform.                                                        ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Load packages
library(dplyr)
library(tidyr)


## Set the work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Exploratory-Data-Analysis/Course_Project_1")

zipFile <- "./data/household_power_consumption.zip"

## Set link and file
lk <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

## Download
if(!dir.exists("./data")){  ## Checks if the data directory exists
        dir.create("data")  ## Creates the folder if it doesn't exist
}

## Checks if the zip file exists, otherwise it starts downloading it
if(!file.exists("./data/household_power_consumption.zip")){
        download.file(url = lk,
                      destfile = zipFile,
                      mode = "wb")
        
        ## Unzip the file
        unzip(zipfile = zipFile,
              exdir = "./data")
}

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

## Save the new data frame
write.csv(df, 
          "C:/Data-Science-Foundations-using-R-Specialization/test.csv", 
          row.names = FALSE)

## Making Plots ##
hist(df$Global_active_power,  ## Set plot1
     col = "red",
     main = "Global Active Power",
     xlab = "Global Active Power (Kilowatts)")

## Save plot1 as png file
dev.copy(png, "C:/Data-Science-Foundations-using-R-Specialization/Exploratory-Data-Analysis/Course_Project_1/plot1.png",
         width = 480,   ## These lines are not necessary 
         height = 480)  ## because the pattern is 480x480 px

dev.off()

