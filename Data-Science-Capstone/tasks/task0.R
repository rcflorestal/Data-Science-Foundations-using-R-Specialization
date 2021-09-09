## Data Science Capstone ##
#
# Tasks
#
## Load packages
## Set the work directory
setwd('C:/Data-Science-Foundations-using-R-Specialization/Data-Science-Capstone/tasks/')

##-------------------------------** Task 0 **---------------------------------##
#
## Get the data set
if(!dir.exists('./data')){   # Check out if the directory data exist
        dir.create('./data') # Create the data directory
}

if(!file.exists('./data/Coursera-SwiftKey.zip')){ # Check out if the file exist
        # Download the data set
        download.file(  
                url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',
                destfile = './data/Coursera-SwiftKey.zip',
                mode = 'wb'
        )
        # Unzip the files
        unzip(zipfile = './data/Coursera-SwiftKey.zip', exdir = './data')
}

## Starts a connection with the file
con <- file('./data/final/en_US/en_US.twitter.txt', open = 'r')

## It's important to close the connection when you are done
close(con)

## Read the first line of the data set
readLines(con, 1)

## Read in the next 5 lines of the data set
readLines(con, 5)


