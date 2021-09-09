##-------------------------------Week 1 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

#' Question 5:
#' The American Community Survey distributes downloadable data about United 
#' States communities. Download the 2006 microdata survey about housing for the 
#' state of Idaho using download.file() from here: 
#' https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
#' using the fread() command load the data into an R object dt
#' The following are ways to calculate the average value of the variable pwgtp15
#' broken down by sex. Using the data.table package, which will deliver the 
#' fastest user time? 
#' mean(DT[DT$SEX == 1, ]$pwgtp15);mean(DT[DT$SEX == 2, ]$pwgtp15)
#' mean(DT$pwgtp15, by = DT$SEX)
#' sapply(split(DT$pwgtp15, DT$SEX), mean)
#' DT[ ,mean(pwgtp15), by = SEX]
#' tapply(DT$pwgtp15, DT$SEX, mean)
#' rowMeans(DT)[DT$SEX == 1] ; rowMeans(DT)[DT$SEX == 2]

# Load package
library(data.table)

# Sets link and file directory
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

csv_file <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-1-Quiz/Fss_pid.csv"

# Download csv file
download.file(url = lk,
              destfile = csv_file)

# Reads the csv file and convert it into a R object
DT <- fread(
            csv_file,
            sep = "auto",
            sep2 = "auto",
            dec = ".",
            header = "auto",
            skip="__auto__"
            )

head(DT)

# system.time(mean(DT[DT$SEX == 1,]$pwgtp15)); system.time(mean(DT[DT$SEX == 2, ]$pwgtp15))
# system.time(mean(DT$pwgtp15, by = DT$SEX))
# system.time(sapply(split(DT$pwgtp15, DT$SEX), mean))
# system.time(DT[ ,mean(pwgtp15), by = SEX])
# system.time(tapply(DT$pwgtp15, DT$SEX, mean))
# system.time(rowMeans(DT)[DT$SEX == 1]); system.time(rowMeans(DT)[DT$SEX == 2])
time1 <- system.time(DT[,mean(pwgtp15),by=SEX])
time2 <- system.time(mean(DT[DT$SEX==1,]$pwgtp15)) + system.time(mean(DT[DT$SEX==2,]$pwgtp15))
time3 <- system.time(mean(DT$pwgtp15,by=DT$SEX))
time4 <- system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
time5 <- system.time(tapply(DT$pwgtp15,DT$SEX,mean))
time6 <- system.time(rowMeans(DT)[DT$SEX==1]) + system.time(rowMeans(DT)[DT$SEX==2])
time1
time2
time3
time4
time5
time6
