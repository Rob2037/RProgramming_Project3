#### Project: Project 3 Coursera 
#### Course: "R Programming"
#### Author: Costa, S. 
#### Date: May, 2014


rankall <- function(outcome, num = "best") {
    
    # Reads the data set 'outcome-of-care-measures.csv' and store in a variable 
    outcomeMeasures <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    # State and Hospital Columns
    stateName = outcomeMeasures[, "State"]
    hospitalName = outcomeMeasures[, "Hospital.Name"]
    
    # Set the position of outcome, and saves the values of the respective column.
    # ignores Warning message: NAs introduced by coercion
    outcomeMeasures[,11] <- suppressWarnings(as.numeric(outcomeMeasures[,11])) ## heart attack
    heartAttack = as.numeric(outcomeMeasures[,11])
    
    outcomeMeasures[,17] <- suppressWarnings(as.numeric(outcomeMeasures[,17])) ## heart failure
    heartFailure = as.numeric(outcomeMeasures[,17])
    
    outcomeMeasures[,23] <- suppressWarnings(as.numeric(outcomeMeasures[,23])) ## pneumonia
    pneumonia = as.numeric(outcomeMeasures[,23])
    
    # dataset based data frame (5 columns)
    outcomeMeasuresDataSet = data.frame(hospitalName, stateName, heartAttack, heartFailure, pneumonia)
    
    # pick a column to select based on the outcome
    if (outcome == "heart attack") {
        outcomeColumn <- "heartAttack"
    } else if (outcome == "heart failure") {
        outcomeColumn <- "heartFailure"
    } else if (outcome == "pneumonia") {
        outcomeColumn <- "pneumonia"
    } else {
        stop("invalid outcome")
    }
    
    
    # check state
    stateDataSet <- split(outcomeMeasuresDataSet[, c("hospitalName", "stateName", outcomeColumn)], outcomeMeasuresDataSet$stateName)
    
    # hospital Rank Function
    hospitalRank <- function(allState, num) {
        stateDataSet <- order(allState[3], allState$hospitalName, na.last = NA)
        
        # figure out indexes for 'best' and 'worst'
        if (num == "best") {
            allState$hospitalName[stateDataSet[1]]
        } else if (num == "worst") {
            allState$hospitalName[stateDataSet[length(stateDataSet)]]
        } else if (is.numeric(num)) {
            allState$hospitalName[stateDataSet[num]]
        } else {
            stop("invalid num")
        }
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    preRankAll <- lapply(stateDataSet, hospitalRank, num)
    
    # #create output dataframe
    rankall <- data.frame(hospital = unlist(preRankAll), state = names(preRankAll), row.names = names(preRankAll))
    
    # return
    rankall
    
}

#****************************************************************#
#*****************************Usage******************************#
##Manual:    
#state="TX" 
#outcome="heart failure"
#num=4

## Run Function:
# tail(rankall("heart failure"), 10)
# tail(rankall("pneumonia", "worst"), 3)
# head(rankall("heart attack", 20), 10)