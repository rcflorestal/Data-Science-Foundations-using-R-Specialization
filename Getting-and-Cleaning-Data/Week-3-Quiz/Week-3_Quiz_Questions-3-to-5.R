##-------------------------------Week 3 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Question 3:
#' Load the Gross Domestic Product data for the 190 ranked countries in this
#' data set:
#' https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
#' 
#' Load the educational data from this data set:
#' https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
#' 
#' Match the data based on the country shortcode. How many of the IDs match? 
#' Sort the data frame in descending order by GDP rank (so United States is 
#' last). What is the 13th country in the resulting data frame?

#' ## Question 4:
#' What is the average GDP ranking for the "High income: OECD" and "High income:
#' nonOECD" group? 
#
#' ## Wuestion 5
#' Cut the GDP ranking into 5 separate quantile groups. Make a table versus
#' Income.Group. How many countries are Lower middle income but among the 38 
#' nations with highest GDP?

## Load libraries
library(dplyr)
library(tidyr)

## Set work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz")

## Set url links and files
lk_gross <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

csvGross <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz/grossDomesticProducts.csv"

lk_edu <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

csvEdu <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz/eduData.csv"

## Download
if(!file.exists(c("grossDomesticProducts.csv", "eduData.csv"))){
        download.file(url = lk_gross,
                      destfile = csvGross)
        download.file(url = lk_edu,
                      destfile = csvEdu)
}


## Load gross data frame
gross <- read.csv(file = csvGross,
                  header = TRUE,
                  sep = ",",
                  dec = ".",
                  comment.char = "",
                  skip = 4,
                  blank.lines.skip = TRUE,
                  stringsAsFactors = FALSE)

## Read gross data frame
#head(gross)

gross <- gross %>% 
        ## Select the columns
        select(1:5 & !3) %>%
        
        ## Set the column names of gross data frame
        rename("CountryCode" = X, 
               "Ranking" = X.1, 
               "Country" = X.3, 
               "Economy.US.dollars" = X.4)

## Display the data frame
as_tibble(gross)
      
## Load educational data frame
edu <- read.csv(file = csvEdu,
                  header = TRUE,
                  sep = ",",
                  dec = ".",
                  comment.char = "",
                stringsAsFactors = FALSE)

## Read gross data frame
as_tibble(edu)

## Join data frames
newDf <- gross %>%
        drop_na() %>%
        filter(!Ranking == "") %>%
        inner_join(edu, 
                   by = "CountryCode") %>%
        arrange(desc(Ranking), na.rm = TRUE)
        
       
## Read the new data frame        
as_tibble(newDf)

### Question 3:
###  Get the number of the IDs match
cat(nrow(newDf), "matches")

### What is the 13th country in the resulting data frame?
newDf %>%
        filter(!Ranking == "") %>%
        arrange(desc(as.numeric(Ranking)), na.rm = TRUE) %>%
        slice(13) %>%
        select(Country) %>%
        as_tibble()


### Question 4:
### What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?  
newDf %>%
        select(1:6) %>%
        filter(Income.Group %in% c("High income: OECD", 
                                   "High income: nonOECD")) %>%
        group_by(Income.Group) %>%
        summarize(avg = mean(as.numeric(Ranking)))


### Question 5:
### Cut the GDP ranking into 5 separate quantile groups. Make a table versus
### Income.Group. How many countries are Lower middle income but among the 38
### nations with highest GDP?
newDf$quant = cut(as.numeric(newDf$Ranking), 
                  breaks = quantile(as.numeric(newDf$Ranking), 
                                    probs = seq(0, 1, 0.2), 
                                    na.rm = TRUE))

### Create table
table(newDf$quant, newDf$Income.Group)


