##-------------------Peer Graded Assignment Course Project 2------------------##
##                                                                            ##
##                      Fine particulate matter (PM2.5)                       ##
##                                                                            ##
##  This script is part of the Getting and Cleaning Data Module of the Data   ##
##  Science Specialization, offered by Johns Hopkins University through       ##
##  Coursera platform.                                                        ##
##                                                                            ##
##----------------------------------------------------------------------------##
#
## Load Packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

## Set the work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Exploratory-Data-Analysis/Course_Project_2")

## Set link 
lk <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

## Set file
zipFile <- "./data/NEI_data.zip"

## Download
if(!dir.exists("./data")){  ## Checks if the data directory exists
        dir.create("data")  ## Creates the folder if it doesn't exist
}

## Checks if the zip file exists, otherwise it starts downloading it
if(!file.exists("./data/NEI_data.zip")){
        download.file(url = lk,
                      destfile = zipFile,
                      mode = "wb")
        
        ## Unzip the file
        unzip(zipfile = zipFile,
              exdir = "./data")
}

## List files and directories
list.files(full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

## Loading the data
NEI <- readRDS("./data/Source_Classification_Code.rds")

SCC <- readRDS("./data/summarySCC_PM25.rds")

## Get more information about the data
str(NEI)

str(SCC)

## Questions
## You must address the following questions and tasks in your
## exploratory analysis. For each question/task you will need
## to make a single plot. Unless specified, you can use any 
## plotting system in R to make your plot.
#
##' 1. Have total emissions from PM2.5 decreased in the United States
##' from 1999 to 2008? 
##' Using the base plotting system, make a plot showing the total 
##' PM2.5 emission from all sources for each of the years 1999, 2002, 
##' 2005, and 2008.
## 
## Summary data - Emissions by year
pm25Year <- tapply(SCC$Emissions, SCC$year, sum)

## Open the graphics devices to save the plot1
png("plot1.png",
    width = 480,
    height = 480)

## Set plot1
barplot(pm25Year,
        main = "Total of PM2.5 Emissions Over Year",
        xlab = "Years",
        ylab = "PM25 emissions (ton)")

## Close the graphics devices
dev.off()

##' 2. Have total emissions from PM2.5 decreased in the Baltimore City,
##' Maryland (fips == "24510") from 1999 to 2008?
##' Use the base plotting system to make a plot answering this question.
##
## Filter data 
baltimore <-  SCC[which(SCC$fips == 24510), ]

## Summary data - Emissions by year
baltimore_pm25 <- tapply(baltimore$Emissions, baltimore$year, sum)

## Open the graphics devices to save the plot2
png("plot2.png",
    width = 480,
    height = 480)

## Set plot2
barplot(baltimore_pm25,
        main = "Total of PM2.5 Emissions in Baltimore City, MD.",
        xlab = "Years",
        ylab = "PM25 emissions (ton)")        

## Close the graphics devices
dev.off()

##' 3. Of the four types of sources indicated by the 'type' (point, nonpoint,
##' onroad, nonroad) variable, which of these four sources have seen
##' decreases in emissions from 1999–2008 for Baltimore City? 
##' Which have seen increases in emissions from 1999–2008? 
##' Use the ggplot2 plotting system to make a plot answer this question.
##
## Open the graphics devices to save the plot3
png("plot3.png",
    width = 480,
    height = 480)

## Set the plot3
SCC %>%
        filter(fips == "24510") %>%
        select(Emissions, year, type) %>%
        group_by(year, type) %>%
        summarize(pm25 = sum(Emissions) / 1000) %>%
        ggplot(aes(year, pm25, color = type)) +
        geom_line() +
        geom_text(aes(label = round(pm25, digits = 2))) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = expression("Emissions of PM"[2.5]* " in Baltimore City by Type, 1999-2008."),
             x = "Years",
             y = expression("Total of  PM"[2.5]* "  (kiloton)"))
        
## Close the graphics devices
dev.off()

##' 4. Across the United States, how have emissions from coal
##' combustion-related sources changed from 1999–2008?
##
## Open the graphics devices to save the plot4
png("plot4.png",
    width = 480,
    height = 480)

## Filter and summary the data
NEI_coal <- NEI %>%
        left_join(SCC, by = "SCC") %>%
        filter(str_detect(EI.Sector, "Coal")) %>%
        group_by(year) %>%
        summarize(Emissions = sum(Emissions) / 1000) 
       
## Set plot4
ggplot(na.omit(NEI_SCC), aes(factor(year), Emissions)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total emissions from coal sources from 1999 to 2008",
             x = "Years",
             y = expression("Total of  PM"[2.5]* "  (kiloton)"))

## Close the graphics devices
dev.off()

##' 5. How have emissions from motor vehicle sources changed from 
##' 1999–2008 in Baltimore City?
##
## Open the graphics devices to save the plot5
png("plot5.png",
    width = 480,
    height = 480)

## Filter and summary the data
NEI_motor <- NEI %>%
        left_join(SCC, by = "SCC") %>%
        filter(str_detect(EI.Sector, "Vehicle") & fips == "24510") %>%
        group_by(year) %>%
        select(year, Emissions) %>%
        summarize(Emissions = sum(Emissions))

## Set plot5
ggplot(na.omit(NEI_motor), aes(factor(year), Emissions)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        labs(title = "Total emissions from Motor Vehicle source",
             subtitle = "From 1999 to 2008 in Baltimore City",
             x = "Years",
             y = expression("Total of  PM"[2.5]* "  (ton)"))

## Close the graphics devices
dev.off()

##' 6. Compare emissions from motor vehicle sources in Baltimore
##' City with emissions from motor vehicle sources in Los Angeles County,
##' California (fips == "06037"). Which city has seen greater changes
##' over time in motor vehicle emissions? 
##
## Open the graphics devices to save the plot6
png("plot6.png",
    width = 480,
    height = 480)

## Filter and summarize the data
NEI_Balt_LA <- NEI %>%
        left_join(SCC, by = "SCC") %>%
        filter(str_detect(EI.Sector, "Vehicle")) %>%
        filter(fips == "24510" | fips == "06037") %>%
        select(fips, year, Emissions) %>%
        group_by(fips, year, Emissions)
        
## Set new facet label to the cities
cities <- c("Los Angeles", "Baltimore")
names(cities) <- c("06037", "24510") 

## Set plot6
ggplot(NEI_Balt_LA, aes(x = factor(year), y = Emissions)) +
        facet_wrap(~ fips, 
                   scales = "free_y",
                   labeller = labeller(fips = cities)) +
        geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        labs(title = "Total emissions from Motor Vehicle source",
             subtitle = "From 1999 to 2008 in Baltimore and Los Angeles Cities",
             x = "Years",
             y = expression("Total of  PM"[2.5]* "  (ton)"))

## Close the graphics devices
dev.off()
