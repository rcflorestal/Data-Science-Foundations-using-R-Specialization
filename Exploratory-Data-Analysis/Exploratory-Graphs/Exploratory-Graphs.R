
##------------------------Exploratory Data Analysis---------------------------##
##                                                                            ##
##                           Exploratory Graphs                               ##
##                                                                            ##
##  This script is part of the Getting and Cleaning Data Module of the Data   ##
##  Science Specialization, offered by Johns Hopkins University through       ##
##  Coursera platform.                                                        ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Load packages


## In this lesson, we'll discuss why graphics are an important tool for data 
## scientists and the special role that exploratory graphs play in the field.

## Set the work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Exploratory-Data-Analysis/Exploratory-Graphs")

## Load the pollution data frame 
pollution <- read.csv(file = "./data/pollution.csv",
         header = TRUE,
         colClasses = c("numeric", "character", "factor", "numeric", "numeric"))

## Read the data frame
head(pollution)

## Exploratory graphs serve mostly the same functions as graphs. They help us 
## find patterns in data and understand its properties. They suggest modeling 
## strategies and help to debug analyses. We DON'T use exploratory graphs to
## communicate results.
#
## Instead, exploratory graphs are the initial step in an investigation, 
## the "quick and dirty" tool used to point the data scientist in a 
## fruitful direction.
#
## The first technique uses the R command summary, a 5-number summary which 
## returns 6 numbers. Run it now with the pm25 column of pollution as its argument.
#
summary(pollution$pm25)

## This shows us basic info about the pm25 data, namely its Minimum
## (0 percentile) and Maximum (100 percentile) values, and three Quartiles
## of the data. These last indicate the pollution measures at which 25%, 50%,
## and 75% of the counties fall below. 
## In addition to these 5 numbers we see the Mean or average measure of 
## particulate pollution across the 576 counties.
#
## Now we'll plot a picture, specifically a boxplot.
ppm <- pollution$pm25

boxplot(ppm, col = "grey")

## The boxplot shows us the same quartile data that summary and quantile did.
## The lower and upper edges of the blue box respectively show the values of 
## the 25% and 75% quantiles.
#

## Add a line to the boxplot
abline(h=12)

## Now use the R command hist
hist(ppm, col = "wheat")

## The histogram gives us a little more detailed information about our data,
## specifically the distribution of the pollution counts, or
## how many counties fall into each bucket of measurements.
#
## Now run the R command rug
rug(ppm)

## Now rerun hist with 3 arguments, ppm as its first, col equal to "green",
## and the argument breaks equal to 100.
#
hist(ppm, col = "grey", breaks = 100)

## The breaks argument specifies the number of buckets to split the data into
#
## Now run the command abline with the argument v equal to 12 and the
## argument lwd equal to 2.
#
abline(v = 12, lwd = 2)

## See the vertical line at 12? Not very visible, is it, even though
## you specified a line width of 2? Run abline with the argument v equal to
## median(ppm), the argument col equal to "magenta", and the argument lwd
## equal to 4.
#
abline(v = median(ppm), col = "magenta", lwd = 4)

## Now recall that our pollution data had 5 columns of information.
## So far we've only looked at the pm25 column. We can also look at
## other information. To remind yourself what's there run the R command
## names with pollution as the argument.
#
names(pollution)

## Let's look at the region column to see what's there.
reg <- table(pollution$region); reg

## Lot more counties in the east than west. We'll use the R command barplot
## to plot this information.
#
barplot(reg, col = "wheat", main = "Number of Counties in Each Region")

## We use the R formula y ~ x to show that y (in this case pm25)
## depends on x (region).
#
boxplot(pm25 ~ region, data = pollution, col = "red")

## Two for the price of one! Similarly we can plot multiple histograms
## in one plot, though to do this we have to use more than one R command.
#
## First we have to set up the plot window with the R command par hich
## specifies how we want to lay out the plots, say one above the other.
## We also use par to specify margins, a 4-long vector which indicates
## the number of lines for the bottom, left, top and right. Type the R 
## command par(mfrow=c(2,1),mar=c(4,4,2,1)) now. Don't expect to see 
## any new result.
#
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

## So we set up the plot window for two rows and one column with
## the mfrow argument. The mar argument set up the margins.
#
## Before we plot the histograms let's explore the R command subset which,
## not surprisingly, "returns subsets of vectors, matrices or data frames which
## meet conditions". We'll use subset to pull off the data we want to plot.
## Call subset now with pollution as its first argument and a boolean
## expression testing region for equality with the string "east". 
## Put the result in the variable east.
#
east <- subset(pollution, region == "east")

head(east)

## So east holds more information than we need. We just want to plot a
## histogram with the pm25 portion. Call hist now with the pm25
## portion of east as its first argument and col equal to "green" as its second.
#
hist(east$pm25, col = "green")

## See? The command par told R we were going to have one column with 2 rows, 
## so it placed this histogram in the top position.
#
## Plot the histogram of the counties from the west using just one R command.
## Let the appropriate subset command (with the pm25 portion specified) be
## the first argument and col (equal to "green") the second.
# 
## To cut down on your typing, use the up arrow key to get your last command 
## and replace "east" with the subset command. Make sure the boolean argument
## checks for equality between region and "west".
#
hist(subset(pollution, region == "west")$pm25, col ="green")


## Let's look at some scatter plots now.
## Scatter plots are two-dimensional plots which show the relationship
## between two variables, usually x and y.
#
## Let's look at a scatterplot showing the relationship between latitude
## and the pm25 data
#
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))

with(pollution, plot(latitude, pm25,
                     main = "Air Pollution in the United States",
                     xlab = "Latitude",
                     ylab = "Fine Particle Pollution (PM2.5)"))

## Note that the first argument is plotted along the x-axis and the second 
## along the y. Now use abline to add a horizontal line at 12. 
## Use two additional arguments, lwd equal to 2 and lty also equal to 2.
#
abline(h = 12, lwd = 2, lty = 2)


## See how lty=2 made the line dashed? Now let's replot the scatterplot.
## This time, call plot directly with 3 arguments.
## The first 2 are pollution$latitude and ppm. The third argument, col, 
## we'll use to add color and more information to our plot.
## Set this argument (col) equal to pollution$region and see what happens.
#
with(pollution, plot(latitude, pm25, 
                     col = region,
                     main = "Air Pollution in the United States",
                     xlab = "Latitude",
                     ylab = "Fine Particle Pollution (PM2.5)"))

legend("topright", pch = 1, col = c("black", "red"), legend = c("East", "West"))

## We've got two colors on the map to distinguish between counties 
## in the east and those in the west. Can we figure out which color is
## east and which west? See that the high (greater than 50) and low
## (less than 25) latitudes are both red. Latitudes indicate distance
## from the equator, so which half of the U.S. (east or west)
## has counties at the extreme north and south?
## >> west

## As before, use abline to add a horizontal line at 12. 
## Use two additional arguments, lwd equal to 2 and lty also equal to 2.
#
abline(h = 12, lwd = 2, lty = 2)

## We see many counties are above the healthy standard set by the EPA,
## but it's hard to tell overall, which region, east or west, is worse.
## Let's plot two scatterplots distinguished by region.
#
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))

## For the first scatterplot, on the left, we'll plot the latitudes and pm25 
## counts from the west. We already pulled out the information for the counties 
## in the east. Let's now get the information for the counties from the west. 
## Create the variable west by using the subset command with pollution as the 
## first argument and the appropriate boolean as the second.
#
west <- subset(pollution, region == "west")

plot(west$latitude, west$pm25, main = "West",
     xlab = "Latitude",
     ylab = "Fine Particle Pollution (PM2.5)")

abline(h = 12, lwd = 2, lty = 2, col = "red")

plot(east$latitude, east$pm25, main = "East",
     xlab = "Latitude",
     ylab = "Fine Particle Pollution (PM2.5)")

abline(h = 12, lwd = 2, lty = 2, col = "red")

## It looks like there are more dirty counties in the east but the extreme
## dirt (greater than 15) is in the west.


