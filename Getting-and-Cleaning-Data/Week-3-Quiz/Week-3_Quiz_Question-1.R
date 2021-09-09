##-------------------------------Week 3 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Question 1:
#' The American Community Survey distributes downloadable data about United 
#' States communities. Download the 2006 microdata survey about housing for the 
#' state of Idaho using download.file() from here:
#' https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
#' 
#' and load the data into R. The code book, describing the variable names is here:
#' https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
#' 
#' Create a logical vector that identifies the households on greater than 10 
#' acres who sold more than $10,000 worth of agriculture products. Assign that 
#' logical vector to the variable agricultureLogical. Apply the which() function 
#' like this to identify the rows of the data frame where the logical vector is 
#' TRUE.

## which(agricultureLogical)

## What are the first 3 values that result?

## Set work Directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz")

## Set url links and files
lk_csv <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
agr_df <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz/agr_df.csv"

lk_pdf <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
codeBook <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz/FPUMSDataDict06.pdf"

## Downloads
if(!file.exists(c("agr_df.csv","FPUMSDataDict06.pdf"))){
        download.file(url = lk_csv,
                      destfile = agr_df)
        download.file(url = lk_pdf,
                      destfile = codeBook,
                      mode = "wb")
}

## Load data
df <- read.csv(file = agr_df,
               header = TRUE,
               sep = ",",
               dec = ".",
               comment.char = "")

## Read data frame
head(df)

## Get more information about the data frame
str(df)

## Create a logical vector that identifies the households on greater than 10 
## acres who sold more than $10,000 worth of agriculture products
agricultureLogical <- df$ACR == 3 & df$AGS == 6
which(agricultureLogical) 
