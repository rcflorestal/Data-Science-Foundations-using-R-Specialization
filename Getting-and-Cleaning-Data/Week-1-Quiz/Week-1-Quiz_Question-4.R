##-------------------------------Week 1 Quiz----------------------------------##
##                                                                            ##
## This script is part of the Getting and Cleaning Data Module of the Data    ##
## Science Specialization, offered by Johns Hopkins University through        ##
## Coursera platform.                                                         ##
##                                                                            ##
##----------------------------------------------------------------------------##

#' Question 4:
#' 4 - Read the XML data on Baltimore restaurants from here:
#' https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml 
#' How many restaurants have zipcode 21231?


# Load package
library(XML)

# Read XML
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
fileURL <- sub('https', 'http', fileURL)

# Parse the XML file
# The xmlParse function returns an object of the XMLInternalDocument class, 
# which is a C-level internal data structure.
xmlDoc <- xmlParse(fileURL)

# Extract the root node form the xml file
# The xmlRoot() function gets access to the root node and its elements. 
rootNode <- xmlRoot(xmlDoc)

# give name of node
xmlName(rootNode)

# how many children in node? (number of nodes in the root)
xmlSize(rootNode) 

# get entire contents of the first element (child) of the root node in the
# xml file
rootNode[[1]]

# name of root's children
xmlName(rootNode[[1]])

# number of nodes in each child
xmlSize(rootNode[[1]])

# Names of the subnodes
xmlApply(rootNode[[1]], xmlName)

# Attributes of the subnodes
xmlSApply(rootNode[[1]], xmlAttrs)

# Size of the subnodes
xmlApply(rootNode[[1]], xmlSize)

# read the contents
xmlApply(rootNode[[1]][[150]], xmlValue)

# xml to dataframe
df <- xmlToDataFrame(getNodeSet(xmlDoc, "//zipcode"))

head(df)

### How many restaurants have zipcode 21231?
summary(df[which(df$text == '21231'), ])


