##-------------------------------Week 2 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

#' The sqldf package allows for execution of SQL commands on R data frames. We 
#' will use the sqldf package to practice the queries we might send with the 
#' dbSendQuery command in RMySQL.



# Question 2
# Download the American Community Survey data and load it into an R object called 
# acs
# link: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv

# Load Package
library(sqldf)

# Set link url and file directory
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

file_csv <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-2-Quiz/getdata_data_ss06pid.csv"

# Set the work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-2-Quiz")

# Download csv file
if(!file.exists("getdata_data_ss06pid.csv")){
        download.file(url = lk,
                      destfile = file_csv)
}

acs <- read.csv(file = file_csv,
                header = TRUE,
                sep = ",",
                dec = ".",
                na.strings = "NA")

##' Which of the following commands will select only the data for the probability
##' weights pwgtp1 with ages less than 50?
##' sqldf("select pwgtp1 from acs")
##' sqldf("select pwgtp1 from acs where AGEP < 50")
##' sqldf("select * from acs where AGEP < 50 and pwgtp1")
##' sqldf("select * from acs")


sqldf("select pwgtp1 from acs where AGEP < 50")   ## This is the correct command
sqldf("select * from acs where AGEP < 50 and pwgtp1") 
sqldf("select * from acs")


##' Using the same data frame you created in the previous problem, what is the
##' equivalent function to unique(acs$AGEP)
##' sqldf("select distinct pwgtp1 from acs")
##' sqldf("select distinct AGEP from acs")
##' sqldf("select AGEP where unique from acs")


sqldf("select distinct pwgtp1 from acs")
sqldf("select distinct AGEP from acs")          ## This is the correct command
sqldf("select AGEP where unique from acs")
sqldf("select unique AGEP from acs")
