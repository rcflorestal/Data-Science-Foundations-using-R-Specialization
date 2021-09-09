##-------------------------------Week 4 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Question 5

## You can use the quantmod (http://www.quantmod.com/) package to get historical
## stock prices for publicly traded companies on the NASDAQ and NYSE. Use the
## following code to download data on Amazon's stock price and get the times 
## the data was sampled.

## Load packages
library(quantmod)
library(lubridate)

amzn = getSymbols("AMZN", auto.assign = FALSE)
sampleTimes = index(amzn)

class(sampleTimes)
## How many values were collected in 2012? 
get_year <- year(sampleTimes)
summary(get_year == 2012)

## How many values were collected on Mondays in 2012?
wday_set2012 <- wday(sampleTimes) == 2 & year(sampleTimes) == 2012
summary(wday_set2012)
