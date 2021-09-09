##-------------------------------Week 4 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Load the Gross Domestic Product data for the 190 ranked countries in this 
## data set: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv

#
## Original data sources: http://data.worldbank.org/data-catalog/GDP-ranking-table
#


## Load packages
library(dplyr)

## Set work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz")

## Set url link and file
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

gdp_file <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz/GDP.csv"

## Dowonload
if(!file.exists("GDP.csv")){
        download.file(url = lk,
                      destfile = gdp_file)
}

## Load data
GDP <- read.csv(file = gdp_file,
                 sep = ",",
                 skip = 4,
                 comment.char = "#",
                 nrows = 190)

as_tibble(GDP)

GDP190 <- GDP %>%
        select(1:5 & !3) %>%
        rename("CountryCode" = X,
               "Ranking" = X.1,
               "countryNames" = X.3,
               "millions.of.Dollars" = X.4)
        
## Question 2
## Remove the commas from the GDP numbers in millions of dollars and average 
## them. What is the average? 
#
GDP190$millions.of.Dollars <- as.numeric(gsub(",", "", GDP190$millions.of.Dollars,))

as_tibble(GDP190)

mean(GDP190$millions.of.Dollars, na.rm = TRUE)

## Question 3
## what is a regular expression that would allow you to count the number of 
## countries whose name begins with "United"? Assume that the variable with 
## the country names in it is named countryNames. How many countries begin 
## with United? 
#
GDP190 %>%
        filter(str_detect(countryNames, ("^United"))) %>%
        print()
