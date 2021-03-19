rankall <- function(outcome, num="best"){
    # Read .csv file
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check if outcome refers to either heart attack, heart failure or pneumonia,
    # and assign the corresponding column
    if(outcome == "heart attack") colOutcome <- data[,11]
    else if(outcome == "heart failure") colOutcome <- data[,17]
    else if(outcome == "pneumonia") colOutcome <- data[,23]
    else stop("invalid outcome")
    
    # Coerce the column as numeric
    colOutcome <- as.numeric(colOutcome)
    
    # Retrieve the hospital name, corresponding state, and corresponding 30-day 
    # mortality rate of the specified outcome.
    final_data <- cbind(data['Hospital.Name'], data['State'], Outcome=colOutcome)
    
    # Sort and return all hospitals in the state, ranked by the outcome
    result <- final_data[order(final_data['Outcome'], final_data['Hospital.Name']), ]
    
    # Split the data according to states
    sep <- split(result, result$State)
    
    # Return nth-ranking hospital for each state 
    out <- sapply(sep, function(x, num) {
        # Assign a value for "best" and "worst", and check if num is valid
        if(num == "best") num <- 1
        else if(num == "worst") num <- nrow(x)
        else if(num > nrow(x)) stop("num > no. of rows")
        
        return(x$Hospital.Name[num])
    }, num)
}