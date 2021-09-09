##-------------------------------Week 4 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## The American Community Survey distributes downloadable data about United
## States communities. Download the 2006 microdata survey about housing for the
## state of Idaho using download.file() from here: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
## and load the data into R. The code book, describing the variable names
## is here: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf

## Load Package
library(dplyr)

## Set work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz")

## Set url links and files
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

hidFile <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz/hid.csv"

lk_code <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"

codeBook <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Quiz/codeBook_hid.pdf"

## downloads
if(!file.exists("hid.csv")){
        download.file(url = lk,
                      destfile = hidFile)
}

if(!file.exists("codeBook_hid.pdf")){
        download.file(url = lk_code,
                      destfile = codeBook,
                      mode = "wb")
}

## Load data
hidDF <- read.csv(file = hidFile,
         header = TRUE)

## Get more information about the data frame
str(hidDF)

## Apply strsplit() to split all the names of the data frame on the characters
## "wgtp". What is the value of the 123 element of the resulting list?
#
strsplit(names(hidDF), split = "\\wgtp+")[123]


