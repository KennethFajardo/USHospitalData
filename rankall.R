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
    
    splited = split(result, result$State)
    ans = sapply(splited, function(x, num) {
        # Order by Deaths and then HospitalName
        x = x[order(x$Outcome, x$Hospital.Name),]
        
        # Return
        if(class(num) == "character") {
            if(num == "best") {
                return (x$Hospital.Name[1])
            }
            else if(num == "worst") {
                return (x$Hospital.Name[nrow(x)])
            }
        }
        else {
            return (x$Hospital.Name[num])
        }
    }, num)
    
    
}