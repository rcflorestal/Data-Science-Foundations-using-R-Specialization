##------------------------Getting and Cleaning Data---------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Summarizing Data.

## Set work directory.
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/summarizing-data")

## Download the data frame.
## Set link.
lk <- "https://data.baltimorecity.gov./api/views/k5ry-ef3g/rows.csv?accsessType=DOWNLOAD"

## Set file.
file_df <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/summarizing-data/restBalt.csv"

## Download.
if(!file.exists("restBalt.csv")){
        download.file(url = lk,
                      destfile = file_df,
                      method = "wininet")
}

## Read data
df <- read.csv(file = file_df,
               header = TRUE,
               sep = ",",
               dec = ".")

## Show the first three rows of the data frame. 
head(df, 3)

## Show the last three rows of the data frame. 
tail(df, 3)

## Basic statistic.
summary(df)

## Get more information about the data frame.
str(df)

## Quantiles of quantitative variables.
quantile(df$councilDistrict, na.rm = TRUE)

quantile(df$councilDistrict, probs = c(0.5, 0.75, 0.9))

## Makes table.
table(df$zipCode, useNA = "ifany")

table(df$councilDistrict, df$zipCode)

## Checks for missing values in the data frame.
sum(is.na(df$councilDistrict))

## Checks if there are any NA values in the councilDistrict column.
any(is.na(df$councilDistrict))

## Checks if there are any zipCode with negative value.
all(df$zipCode > 0)

## Sum NA values in each column.
colSums(is.na(df))

## Check if there are no NA value in all columns. 
all(colSums(is.na(df)) == 0)

## Values with specific characteristics.
## Checks how many restaurants there are in the zipcode 21212.
table(df$zipCode %in% c("21212")) 

## Checks how many restaurant there are in the zipcodes 21212 and 21213.
table(df$zipCode %in% c("21212", "21213"))

## Creates a new data frame from the statement above
df[df$zipCode %in% c("21212", "21213"), ]

## Cross tabs
data(UCBAdmissions)
DF = as.data.frame(UCBAdmissions)
summary(DF)

xt <- xtabs(Freq ~ Gender + Admit, data = DF)
xt

## Size of a data set
fakeData = rnorm(1e5)
object.size(fakeData)

print(object.size(fakeData), units = "Mb")

## Creating Sequences
s1 <- seq(1, 10, by = 2); s1

s2 <- seq(1, 10, length = 3); s2

x <- c(1, 3, 8, 25, 100); seq(along = x)

## Subsetting variables
df$nearMe = df$neighborhood %in% c("Roland Park", "Homeland")
table(df$nearMe)

## Creating Binary Variables
df$zipWrong = ifelse(df$zipCode < 0, TRUE, FALSE)
table(df$zipWrong, df$zipCode < 0)

## Easier Cutting
library(Hmisc)
df$zipGroups = cut2(df$zipCode, g = 4)
table(df$zipGroups)

## Creating Factor Variables
df$zcf <- factor(df$zipCode)
df$zcf[1:10]
class(df$zcf)


## Level of Factor Variables
yesno <- sample(c("yes", "no"), size = 10, replace = TRUE)

yesnofac = factor(yesno, levels = c("yes", "no"))

relevel(yesnofac, ref = "yes")

as.numeric(yesnofac)

## Reshape Data
library(reshape2)

head(mtcars)

## Melting Data Frames
mtcars$carName <- rownames(mtcars)
carMelt <- melt(mtcars, 
                id = c("carName", "gear", "cyl"), 
                measure.vars = c("mpg", "hp"))

head(carMelt)
tail(carMelt)

## Casting Data Frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData

cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData

## Averaging Values
head(InsectSprays)

tapply(InsectSprays$count, InsectSprays$spray, sum)

## Another way - split
spIns = split(InsectSprays$count, InsectSprays$spray)
spIns

## Another Way = apply
sprCount = lapply(spIns, sum)
sprCount

## Another way - combine
unlist(sprCount)

sapply(spIns, sum)

## Another way - plyr package
ddply(InsectSprays, .(spray), summarize, sum = sum(count))

## Creating a new variable
spraySums <- ddply(InsectSprays, .(spray), summarize, sum = ave(count, FUN = sum))
dim(spraySums)
head(spraySums)
tail(spraySums)
