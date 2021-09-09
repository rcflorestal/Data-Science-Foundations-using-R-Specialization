# Subsetting Data frame

set.seed(13435)

x <- data.frame("var1" = sample(1:5), 
                "var2" = sample(6:10),
                "var3" = sample(11:15))

x

x <- x[sample(1:5), ]
x

x$var2[c(1, 3)] = NA
x

## Extract the values from fisrt column
x[, 1]  ## The same: x[, "var1]

## Select variables va1 and var2
x[, c("var1", "var2")]

## Extract the values of the rows 1 and 2 from column 2
x[1:2, "var2"]

## Logical and
x[(x$var1 <= 3 & x$var3 > 11), ]

## Logical or
x[(x$var1 <= 3 | x$var3 > 15), ]

## Dealing with missing values
x[which(x$var2 > 8), ]

## Sorting
sort(x$var1)

sort(x$var1, decreasing = TRUE)

sort(x$var2, na.last = TRUE)

## Ordering
x[order(x$var1), ]

x[order(x$var1, x$var3), ]

## Add columns
x$var4 <- rnorm(5)      ## The same: 
x
