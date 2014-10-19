#### Project: Project 3 Coursera 
#### Course: "R Programming"
#### Author: Costa, S. 
#### Date: May, 2014


rankhospital <- function(state, outcome, num = "best") {
    
    ## Reads the data set 'outcome-of-care-measures.csv' and store in a variable 
    outcomeMeasures <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    ## State and Hospital Columns
    stateName = outcomeMeasures[, "State"]
    hospitalName = outcomeMeasures[, "Hospital.Name"]
    
    ## Set the position of outcome, and saves the values of the respective column.
    ## ignores Warning message: NAs introduced by coercion
    outcomeMeasures[,11] <- suppressWarnings(as.numeric(outcomeMeasures[,11])) ## heart attack
    heartAttack = outcomeMeasures[,11]
    
    outcomeMeasures[,17] <- suppressWarnings(as.numeric(outcomeMeasures[,17])) ## heart failure
    heartFailure = outcomeMeasures[,17]
    
    outcomeMeasures[,23] <- suppressWarnings(as.numeric(outcomeMeasures[,23])) ## pneumonia
    pneumonia = outcomeMeasures[,23]
    
    ## dataset based data frame (5 columns)
    outcomeMeasuresDataSet = data.frame(hospitalName, stateName, heartAttack, heartFailure, pneumonia)
    
    ##
    if (outcome == "heart attack") {
        outcomeColumn <- "heartAttack"
    } else if (outcome == "heart failure") {
        outcomeColumn <- "heartFailure"
    } else if (outcome == "pneumonia") {
        outcomeColumn <- "pneumonia"
    } else {
        stop("invalid outcome")
    }
    
    ## check state
    stateDataSet <- outcomeMeasuresDataSet[outcomeMeasuresDataSet$stateName == state, c("hospitalName", outcomeColumn)]
    
    if(nrow(stateDataSet) == 0){
        stop("Invalid state")
    }
    
    stateDataSet[,2] <- as.numeric(stateDataSet[,2])
    stateDataSetRank <- order(stateDataSet[outcomeColumn], stateDataSet$hospitalName, na.last = NA)
        
    ## Loop to return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best") {
        as.character(stateDataSet$hospitalName[stateDataSetRank[1]])
    } else if (num == "worst") {
        as.character(stateDataSet$hospitalName[stateDataSetRank[length(stateDataSetRank)]])
    } else if (is.numeric(num)) {
        as.character(stateDataSet$hospitalName[stateDataSetRank[num]])
    } else {
        stop("invalid num")
    }
    
}

#****************************************************************#
#*****************************Usage******************************#
##Manual:    
#state="TX" 
    #outcome="heart failure"
    #num=4

## Run Function:
## rankhospital("TX", "heart failure", 4)
    #[1] "DETAR HOSPITAL NAVARRO"

## rankhospital("MD", "heart attack", "worst")
    #[1] "HARFORD MEMORIAL HOSPITAL"

## rankhospital("MN", "heart attack", 5000)
    #[1] NA
#****************************************************************#