##-------------------------------Week 4 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Question 4
## Load the Gross Domestic Product data for the 190 ranked countries in 
## this data set: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
## Load the educational data from this data set:
## https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
#
## Original data sources: 
## http://data.worldbank.org/data-catalog/GDP-ranking-table
## http://data.worldbank.org/data-catalog/ed-stats

## Set work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz")

## Set url links and files
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

gdp_file2 <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz/GDP2.csv"

lk_edu <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

edu_file <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz/EDU.csv"

## Dowonloads
if(!file.exists("GDP2.csv")){
        download.file(url = lk,
                      destfile = gdp_file2)
}

if(!file.exists("EDU.csv")){
        download.file(url = lk_edu,
                      destfile = edu_file)
}

## Load GDP data
gdp2 <- read.csv("GDP2.csv", 
                 skip = 4, 
                 nrows = 190) %>%
        select(1:5 & !3) %>%
        rename("CountryCode" = X, 
               "Ranking" = X.1,
               "CountryName" = X.3,
               "millions.of.Dollars" = X.4)


## Read GDP
as_tibble(gdp2)

## Load educational data
edu <- read.csv("EDU.csv")

## Read educational data
as_tibble(edu)

## Match the data based on the country shortcode. Of the countries for which 
## the end of the fiscal year is available, how many end in June?
#
df <- gdp2 %>% 
        left_join(edu, by = "CountryCode") %>%
        filter(str_detect(Special.Notes, ("^Fiscal year end: June 30")))

## Get number of rows
nrow(df)        
