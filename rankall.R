## rankall
## Takes the arguments outcome and num
## outcome : one of three conditions, heart failure, heart attack, or pneumonia
## num : a numerical value or "best" or "worst"
## Takes the outcome argument and searches for the hospital with a rank == num in each US state.
## Returns a data.frame object with the hospital in each state of the given rank num, and orders the frame 
## alphabetically by state.
## If num is NaN and is neither "best" or "worst", then rankall will throw an invalid num error
## If the outcome is none of the three possible outcomes, throws an invalid outcome error

rankall <- function(outcome, num = "best") {
  
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  names <- names(data)
  states <- unique(as.character(data[,7]))
  states <- states[order(states)]
  valid_outcomes <- c("heart attack" = names[11], "heart failure" = names[17], "pneumonia" = names[23])
  
  ## Check that outcomes are valid
  if(!(outcome %in% names(valid_outcomes))) {
    
    stop("invalid outcome")
    
  }
  
  # Sets num = 1 for best, avoid having to check for it 50 times later
  if(num == "best") {
    num = 1
  # If not worst and NaN, then we know that looking for a rank will fail
  } else if(num != "worst" & !is.numeric(num)) {
    stop("num is invalid")
  }
  
  data[,2] <- as.character(data[,2])
  data[,valid_outcomes[outcome]] <- as.numeric(data[,valid_outcomes[outcome]])

  my_outcomes <- data.frame(sapply(states, helpRank, outcome = outcome, 
      num = num, data = data, valid_outcomes = valid_outcomes))
  my_outcomes <- cbind(my_outcomes, states)
  names(my_outcomes)[1] = "hospital"
  names(my_outcomes)[2] = "state"
  my_outcomes
}

## A helper function for rankall. It searches for the hospital of the requested rank num
## in a given state, and returns the name of that hospital to be used by the rankall function
## It is similar to rankhospital, but all data validation is handled by the rankall function to
## avoid unnecessary checks of whether outcome/state/num are valid.

helpRank <- function(state, outcome, num = "best", data, valid_outcomes) {
  
  state_data <- data[data[,7] == state,]
  state_data <- state_data[!is.na(state_data[,valid_outcomes[outcome]]),]

  state_data <- state_data[order(state_data[,valid_outcomes[outcome]], state_data[,2]),]
  if(num == "worst") {

    return(state_data[length(state_data[,2]),2])

  } 
  
  state_data[num, 2]
  
}