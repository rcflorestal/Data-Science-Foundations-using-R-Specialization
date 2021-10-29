##-------------------------Programming Assignment 3---------------------------##
##                                                                            ##
## This script is part of the R-Programming Module of the Data Science        ##
## Specialization, offered by Johns Hopkins University through Coursera       ## 
## platform.                                                                  ##
##                                                                            ##
##----------------------------------------------------------------------------##

#' @description 
#' The function reads the outcome-of-care-measures.csv file and returns a 
#' 2-column data frame containing the hospital in each state that has the
#' ranking specified in num.
#' 
#' @pars
#' outcome - The name mortality rates 
#' num - the ranking of the hospital
#' @details 
#' Read outcome-of-care-measures.csv file
#' @return 
#' Return a data frame containing the names of the hospitals that are the best
#' in their respective states for 30-day heart attack death rates.

## Load packages
library(dplyr)
library(tidyr)
library(stringr)

## Set work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/R-Programming/Programming Assignment 3/")

rankall <- function(outcome, num = "best"){
        ## Read outcome data
        outcomeData <- read.csv(file = "./outcome-of-care-measures.csv",
                                header = TRUE,
                                sep = ",",
                                dec = ".",
                                colClasses = "character",
                                na.strings = "Not Available")
        
        ## Check if outcome is valid
        varList <- c("heart attack", "heart failure", "pneumonia")
        
        if(!outcome %in% varList){
                stop("invalid outcome")
        }
        
        ## Set a new data frame
        outcome <- str_to_title(outcome)
        out <- str_replace(outcome, " +", "_")
        
        df <- data.frame(
                State = outcomeData[, 7],
                Name = outcomeData[, 2],
                Heart_Attack = as.numeric(outcomeData[, 11]),
                Heart_Failure = as.numeric(outcomeData[, 17]),
                Pneumonia = as.numeric(outcomeData[, 23])
        )
        
        ## For each state, find the hospital of the given rank
        if(outcome == "heart attack"){
                df <- df %>% 
                        ## Filter all rows without NA values
                        drop_na() %>%
                        
                        ## Select variables
                        select(Name, State, Heart_Attack) %>%
                        
                        ## Select rate
                        group_by(State) %>%
                        filter(Heart_Attack == num) %>%
                        unique()
                        
                        
                
        }else if(outcome == "heart failure"){
                df <- df %>% 
                        ## Filter all rows without NA values
                        drop_na() %>%
                        
                        ## Select variables
                        select(Name, State, Heart_Failure) %>%
                        
                        ## Select rate
                        group_by(State) %>%
                        filter(Heart_Failure == num) %>%
                        unique()
                            
                        
        }else{
                df <- df %>% 
                        ## Filter all rows without NA values
                        drop_na() %>%
                        
                        ## Select variables
                        select(Name, State, Pneumonia) %>%
                        group_by(State) %>%
                        ## Select rate
                        filter(Pneumonia == num) %>%
                        unique()
                        
                        
                
        }
        
        ## Check parameters
        if(num >= 1 & num <= nrow(df)){
                df <- df[, c(1,2)]
        }
        else if(num == "best"){
                df <- df[which.min(df[, out]), "Name"]
        }
        else if(num == "worst"){
                df <- df[which.max(df[, out]), "Name"]
        }else{
                return(NA)
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        return(df)
}

## Call the rankall function
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
