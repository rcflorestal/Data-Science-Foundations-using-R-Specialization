##-------------------------------Week 1 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

# The American Community Survey distributes downloadable data about United States 
# communities. Download the 2006 microdata survey about housing for the state of 
# Idaho using download.file() from here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
#
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 

#' Question 1:
#' How many properties are worth $1,000,000 or more?

#' Question 2:
#' Use the data you loaded from Question 1. Consider the variable FES in the
#' code book. Which of the "tidy data" principles does this variable violate?


# Set link and directory
# .csv file
lk_data <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

my_fileData <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-1-Quiz/Data_Fss_Hid.csv" 

# Code Book file
lk_codeBook <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"

my_fileCode <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-1-Quiz/FPUMSDataDict.pdf"


# Download files
# .csv file
download.file(url = lk_data,
              destfile = my_fileData)

# Code Book file
download.file(url = lk_codeBook,
              destfile = my_fileCode,
              mode = "wb")

# Question 1
# Read .csv file
housing <- read.csv(file = my_fileData,
                    header = TRUE,
                    sep = ",")

# Get data overview
str(housing)

## How many properties are worth $1,000,000 or more?
nrow(housing[which(housing$VAL >= 24 & !housing$VAL == "NA"), ])
