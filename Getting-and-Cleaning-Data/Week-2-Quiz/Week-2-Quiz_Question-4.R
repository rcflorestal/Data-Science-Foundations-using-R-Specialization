##-------------------------------Week 2 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

#' Question 4
#' How many characters are in the 10th, 20th, 30th and 100th lines of HTML from
#' this page: http://biostat.jhsph.edu/~jleek/contact.html
#' (Hint: the nchar() function in R may be helpful)


# Set url connection
con = "http://biostat.jhsph.edu/~jleek/contact.html"

htmCode = readLines(con)

close(con)

# Read lines from html
nchar(htmCode[10])
nchar(htmCode[20])
nchar(htmCode[30])
nchar(htmCode[100])
