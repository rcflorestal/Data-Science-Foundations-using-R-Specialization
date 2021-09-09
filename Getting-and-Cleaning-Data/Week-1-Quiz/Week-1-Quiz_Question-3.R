##-------------------------------Week 1 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

#' Question 3:
#' Download the Excel spreadsheet on Natural Gas Aquisition Program here: 
#' https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx 
#' Read rows 18-23 and columns 7-15 into R and assign the result to a variable
#' called: dat
#' What is the value of: sum(dat$Zip*dat$Ext,na.rm=T)

# Load package
library(xlsx)

# Set link and file
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

data_file <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-1-Quiz/Data_NGAP.xlsx"

# Download file
download.file(url = lk,
              destfile = data_file,
              mode = "wb")

# Read data
dat <- read.xlsx(file = data_file,
                 sheetIndex = 1,       ## Reads only the spreed sheet 1
                 rowIndex = c(18:23),  ## Reads rows 18 to 23
                 colIndex = c(7:15))   ## Reads cols 18 to 23              

# Answer
sum(dat$Zip*dat$Ext,na.rm=T)    ## 36534720
  
