##--------------Managing Data Frames with dplyr - Basic Tools-----------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Load the dplyr package
library(dplyr)

## Download chicago data frame
## Set link url and file
lk <- "http://www.biostat.jhsph.edu/~rpeng/leanpub/rprog/chicago_data.zip"

chicago_data <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/summarizing-data/chicago_data.zip"

## Set work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/summarizing-data")

## Download
if(!file.exists("chicago_data.zip")){
        download.file(url = lk,
                      destfile = chicago_data,
                      mode = "wb")
}

## unzip file
unzip(zipfile = chicago_data)

## Load the data frame
chicago <- readRDS("chicago.rds")

dim(chicago)    ## Dimensions of the data frame

names(chicago)  ## Get the names of the variables

str(chicago)    ## Get more information about the data frame

head(chicago)   ## Head the data frame

## Head only city and dptp columns
head(select(chicago, city:dptp))

## Head all columns of the data frame except the columns city and dptp
head(select(chicago, -(city:dptp)))

## Equivalent base R:
i <- match("city", names(chicago))
j <- match("dptp", names(chicago))
head(chicago[, -(i:j)])

## Filter the rows values of the variables
chic.f <- filter(chicago, pm25tmean2 > 30)

## Read the first ten rows only from columns 1 to 3 from
head(select(chic.f, 1:3, pm25tmean2), 10) 

## Filter values of the two variables
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
head(select(chic.f, 1:3, pm25tmean2, tmpd), 10)

## Arrange function reordering rows of a data frame (while preserving corresponding
## order of other columns) is normally a pain to do in R.
chicago <- arrange(chicago, date)
head(select(chicago, date, pm25tmean2), 3)
tail(select(chicago, date, pm25tmean2), 3)

## Columns can be arranged in descending order too.
chicago <- arrange(chicago, desc(date))
head(select(chicago, date, pm25tmean2), 3)
tail(select(chicago, date, pm25tmean2), 3)

## Renaming a variable in a data frame in R is surprising hard to do!
head(chicago[, 1:5], 3)
chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
head(chicago[, 1:5], 3)

## Create new variable
chicago <- mutate(chicago,
                  pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
head(select(chicago, pm25, pm25detrend))

## Generating summary statistics by stratum - grouping data
chicago <- mutate(chicago,
                  tempcat = factor(1 * (tmpd > 80),
                                   labels = c("cold", "hot")))

hotcold <- group_by(chicago, tempcat) ## group by temperature
hotcold

summarize(hotcold, pm25 = mean(pm25, na.rm = TRUE),
          o3 = max(o3tmean2),
          no2 = median(no2tmean2))

## create a year varible using as.POSIXlt() and group by year
chicago <- mutate(chicago, 
                  year = as.POSIXlt(date)$year + 1900)

years <- group_by(chicago, year)

summarize(years, pm25 = mean(pm25, na.rm = TRUE),
          o3 = max(o3tmean2, na.rm = TRUE),
          no2 = median(no2tmean2, na.rm = TRUE))

## Pipeline operator %>%
chicago %>%
        mutate(month = as.POSIXlt(date)$mon + 1) %>%
        group_by(month) %>%
        summarize(pm25 = mean(pm25, na.rm = TRUE),
                  o3 = max(o3tmean2, na.rm = TRUE),
                  no2 = median(no2tmean2, na.rm = TRUE))

