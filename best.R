## best
## Takes two args: 
## state : Two character initials for a US state
## outcome : one of three possible outcomes to check for; heart attack, heart failure, or pneumonia
## When provided two valid arguments, finds the name of the hospital in the provided state with the
## lowest 30-day mortality rate for the given outcome and returns this value. If more than one exist,
## finds the hospital whose name comes first alphabetically.
## If the state is invalid, throws an invalid state error.
## If the outcome is invalid, throws an invalid outcome error

best <- function(state, outcome) {

  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  names <- names(data)
  valid_outcomes <- c("heart attack" = names[11], "heart failure" = names[17], "pneumonia" = names[23])
  
  # Error Check; invalid outcome throws invalid outcome error;
  # invalid state initials throw invalid state error
  if(!(outcome %in% names(valid_outcomes))) {
    
    stop("invalid outcome")
    
  } else if( !(state %in% data[,7])) {
    
    stop("invalid state")
    
  }

  # Creates a partition of the dataframe, so we only look at data in the inputted state 
  state_data <- data[data[,7] == state,]
  # Coerces data in the column representing the requested outcome into a numeric type
  state_data[,valid_outcomes[outcome]] <- as.numeric(state_data[,valid_outcomes[outcome]], na.rm = TRUE)
  # Orders the data frame by the value in the outcome column, then if equal, by alphabetical order
  state_data <- state_data[order(state_data[,valid_outcomes[outcome]], state_data[,2]),]
  
  # Returns the hospital who is ranked 1 in the state as detailed above
  state_data[1,2]

}