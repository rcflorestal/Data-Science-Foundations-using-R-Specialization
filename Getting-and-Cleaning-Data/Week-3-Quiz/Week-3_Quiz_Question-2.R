##-------------------------------Week 3 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Question 2:
#' Using the jpeg package read in the following picture of your instructor into 
#' R
#' https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg 
#' 
#' Use the parameter native=TRUE. What are the 30th and 80th quantiles of the 
#' resulting data? (some Linux systems may produce an answer 638 different for 
#' the 30th quantile)

## Load the jpeg package
library(jpeg)

## Set work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz")

## Set url link and file
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"

jpegFile <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-3-Quiz/jpegFile.jpg"

## Download
if(!file.exists("jpegFile.jpg")){
        download.file(url = lk,
                      destfile = jpegFile,
                      mode = "wb")
}

## Read the jpeg file
jpg <- jpeg::readJPEG(source = jpegFile, 
                      native = TRUE)

## Get the quantile
quantile(jpg, probs = c(0.3, 0.8))
