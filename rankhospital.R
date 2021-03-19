rankhospital <- function(state, outcome, num="best"){
    # Read .csv file
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check if outcome refers to either heart attack, heart failure or pneumonia,
    # and assign the corresponding column
    if(outcome == "heart attack") colOutcome <- data[,11]
    else if(outcome == "heart failure") colOutcome <- data[,17]
    else if(outcome == "pneumonia") colOutcome <- data[,23]
    
    # Coerce the column as numeric
    colOutcome <- as.numeric(colOutcome)
    
    # Retrieve the hospital name, corresponding state, and corresponding 30-day 
    # mortality rate of the specified outcome.
    temp <- cbind(data['Hospital.Name'], data['State'], Outcome=colOutcome)
    
    # Remove rows with missing values
    final_data <- temp[complete.cases(temp),]
    # Select data from state
    hospital <- subset(final_data, final_data[,2] == state)
    
    # Sort and return all hospitals in the state, ranked by the outcome
    result <- hospital[order(hospital['Outcome'], hospital['Hospital.Name']), ]
    
    # Get the index of the last row
    last <- nrow(result)
    
    # Add a column for ranking the hospitals
    result <- cbind(result, Rank=1:last)
    
    # Assign a value for "best" and "worst"
    if(num == "best") num <- 1
    else if(num == "worst") num <- last
    else if(num > last) return(NA)
    
    # Get the num-th hospital
    out <- result[result['Rank']==num, 1]
    
    # Return the result
    out
    
}