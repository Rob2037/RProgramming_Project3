#### Project: Project 3 Coursera 
#### Course: "R Programming"
#### Author: Costa, S. 
#### Date: May, 2014


best <- function(state, outcome) {
    ## Reads the data set 'outcome-of-care-measures.csv' and store in a variable 
    outcomeMeasures <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Set the position of outcome, and saves the values of the respective column.
    ## ignores Warning message: NAs introduced by coercion
    outcomeMeasures[,11] <- suppressWarnings(as.numeric(outcomeMeasures[,11])) ## heart attack
    outcomeMeasures[,17] <- suppressWarnings(as.numeric(outcomeMeasures[,17])) ## heart failure
    outcomeMeasures[,23] <- suppressWarnings(as.numeric(outcomeMeasures[,23])) ## pneumonia
    
    # The three types of outcomes are stored in a vector 
    # for subsequent verification of its existence
    outcomeCondition <- c('heart attack', 'heart failure', 'pneumonia')
    
    if (!outcome %in% outcomeCondition) { 
        stop('invalid outcome') 
    }
    
    # choose a outcome column based on result
    if (outcome == 'heart attack' ) { 
        column <- 11 
    }
   
    if (outcome == 'heart failure' ) { 
        column <- 17 
    }
    
    if (outcome == 'pneumonia' ) {
        column <- 23 
    }
    
    
    # the different states are saved in a list 
    # for subsequent verification of its existence
    statesList <- unique(outcomeMeasures$State)
    if (!state %in% statesList) { 
        stop('invalid state') 
    }
    
    # state subset
    outcomeMeasuresByState <- outcomeMeasures[grep(state, outcomeMeasures$State, ignore.case = TRUE),]
    Best <- outcomeMeasuresByState[order(outcomeMeasuresByState[,column],outcomeMeasuresByState[,2]),]

    # return best value
    Best[1,2]

}



