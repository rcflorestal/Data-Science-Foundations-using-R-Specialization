## Data Science Capstone ##

## Load packages
## Set the work directory
setwd('C:/Data-Science-Foundations-using-R-Specialization/Data-Science-Capstone/Quiz_1/')

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

##-------------------------------**. Quiz 1 .**-------------------------------##
#
## Question 1
## Get the size of the en_US.twitter.txt file
#
file.info( 
        './data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt',
        extra_cols = FALSE
)


## Question 2
## The en_US.twitter.txt has how many lines of text?
#
testCon <- file(
        './data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt',
        open = 'r'
)
readSizeOf <- 20000 # assuming you are having 2GB RAM on your PC
nooflines <- 0
( while((linesread <- length(readLines(testCon, readSizeOf))) > 0 )
        nooflines <- nooflines + linesread )
close(testCon)
nooflines

## Question 3
## What is the length of the longest line seen in any of the 
## three en_US data sets? 
#
fileList <- list.files(
        path = './data/', recursive = TRUE, pattern = '.*en_.*.txt'
)

l <- lapply(paste('./data/', fileList, sep = '/'), function(f){
        fileSize <- file.info(f)[1]/1024/1024
        con <- file(f, open = 'r')
        lines <- readLines(con)
        numChars <- lapply(lines, nchar(f, type = 'char'))
        maxChars <- which.max(numChars)
        numWords <- sum(sapply(strsplit(lines, '\\s+'), length))
        close(con)
        return(c(
                f, format(round(fileSize, 2), 
                          nsmall = 2), 
                length(lines), 
                maxChars, 
                numWords)
        )
})

df <- data.frame(matrix(unlist(l), nrow = length(l), byrow = TRUE))
colnames(df) <- c(
        'File', 'Size(MB)', 'Num.of.Lines', 'Longest.Line', 'Num.of.Words'
)
df

## Question 4
## In the en_US twitter data set, if you divide the number of lines 
## where the word "love" (all lowercase) occurs by the number of lines
## the word "hate" (all lowercase) occurs, about what do you get?
#
en_US_twitter <- readLines( # Read the data set
        './data//Coursera-SwiftKey/final/en_US/en_US.twitter.txt'
)

# Apply regex expression to convert characters to lowercase
en_US_twitter <- stringr::str_to_lower(en_US_twitter)

# Take only the wprds love and hate
love <- sum(grepl('love', en_US_twitter))
hate <- sum(grepl('hate', en_US_twitter))
love / hate

## Question 5
## The one tweet in the en_US twitter data set that matches the word 
## "biostats" says what?
#
biostats <- grep('\\bbiostats\\b', en_US_twitter, value = TRUE)
head(biostats)

## Question 6
## How many tweets have the exact characters "A computer once beat me at chess,
## but it was no match for me at kickboxing". 
## (I.e. the line matches those characters exactly.)
#
tweetsComp <- grep('^A computer|kickboxing$', en_US_twitter, value = TRUE)
head(tweetsComp)
