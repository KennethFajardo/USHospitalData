best <- function(state, outcome){
    # Read .csv file
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check if outcome refers to either heart attack, heart failure or pneumonia,
    # and assign the corresponding column
    if(outcome == "heart attack") colOutcome <- data[,11]
    else if(outcome == "heart failure") colOutcome <- data[,17]
    else if(outcome == "pneumonia") colOutcome <- data[,23]
    
    # Retrieve the hospital name, corresponding state, and corresponding 30-day 
    # mortality rate of the specified outcome.
    temp <- cbind(data['Hospital.Name'], data['State'], Outcome=colOutcome)
    
    # Remove rows with missing values
    final_data <- temp[complete.cases(temp),]
    
    # Select data from state
    hospital <- subset(final_data, final_data[,2] == state)
    
    # Select hospital in state where the mortality rate is the minimum
    result <- hospital[as.numeric(hospital[,3]) == min(as.numeric(hospital[,3])), 1]
    
    # Sort and return the first hospital in ascending alphabetical order
    result <- sort(result)
    
    # Return the result
    result[1]
}