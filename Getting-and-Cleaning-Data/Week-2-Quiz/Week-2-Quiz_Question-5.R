##-------------------------------Week 2 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

#' Read this data set into R and report the sum of the numbers in the fourth of 
#' the nine columns. 
#' https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for
#' Original source of the data: http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for
#' (Hint this is a fixed width file format)
 

# Set link and file directory
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"

for_file <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-2-Quiz/data_Fwksst8110.for"

# Set the work dictory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-2-Quiz")

# Download .for file
if(!file.exists("data_Fwksst8110.for")){
        download.file(url = lk,
                      destfile = for_file,
                      mode = "wb")
}

# Read data
df <- read.fwf(file = 'data_Fwksst8110.for',
               widths = c(-1,9,    ## Read the first column
                          -5,4,4,  ## Read the second and third columns
                          -5,4,4,  ## Read the fourth and fifth columns
                          -5,4,4,  ## Read the sixth and seventh columns
                          -5,4,4), ## Read the eighth and ninth columns
               col.names = c("week","nino1and2sst","nino1and2ssta","nino3sst",
                             "nino3ssta","nino34sst","nino34ssta",
                             "nino4sst","nino4ssta"),
               skip = 4,
               strip.white = TRUE)


head(df, 10)
tail(df)
str(df)


## Check the number of rows and columns
dim(df)

## Select the fourth column (nino3sst) from the nine columns in the data frame,
## and Sum the numbers of this column.
sum(df[, 4])  ## 32426.7
