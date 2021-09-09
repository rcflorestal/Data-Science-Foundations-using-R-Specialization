##------------------------Exploratory Data Analysis---------------------------##
##                                                                            ##
##                            Plotting Systems                                ##
##                                                                            ##
##  This script is part of the Getting and Cleaning Data Module of the Data   ##
##  Science Specialization, offered by Johns Hopkins University through       ##
##  Coursera platform.                                                        ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Load packages
library(ggplot2)
library(lattice)
library(jpeg)

## Loaded and read the dataset cars
data(cars)

head(cars)

## Plot
with(cars, plot(speed, dist))

## text function
text(mean(cars$speed), max(cars$dist), "SWIRL rules!")

## Loaded and read the dataset state from lattice package
state = data.frame(state.x77, region = state.region)

head(state)

## Create a table with the argument state$region to see how many categories
## there are and how many states are in each.
#
table(state$region)

## Let's use the lattice command xyplot to see how life
## expectancy varies with income in each of the four regions.
#
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))


## We see the data for each of the 4 regions plotted in one row. 
## Based on this plot, which region of the U.S. seems to have the shortest
## life expectancy?
# >>> SOUTH

## Although ggplot2 bears a superficial similarity to lattice, 
## it's generally easier and more intuitive to use. Its default mode makes
## many choices for you but you can still customize a lot. 
## The package is based on a "grammar of graphics" (hence the gg in the name),
## so you can control the aesthetics of your plots. 
## For instance, you can plot conditioning graphs and panel plots as we 
## did in the lattice example.
#
## We'll see an example now of ggplot2 with a simple (single) command.
## Loaded and read the mpg dataset from the ggplot2 package.
## This mpg data holds fuel economy data between 1999 and 2008 for 38 
## different models of cars.
#
data(mpg)
head(mpg)

## Run dim with the argument mpg to see how big the dataset is.
dim(mpg)

## How many models of cars we're dealing with?
table(mpg$model)

## We're interested in the effect engine displacement (displ) has on highway
## gas mileage (hwy), so we'll use the ggplot2 command qplot to display this 
## relationship
#
qplot(displ, hwy, data = mpg)
