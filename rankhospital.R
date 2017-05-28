## rankhospital
## takes the args state, outcome, and num
## state : Two character abbreviation for US state, e.g., "WA"
## outcome : one of heart attack, heart failure, or pneumonia
## num : "best", "worst", or an integer value
## Takes the three arguments. Finds the hospital in the inputted state with 
## its mortality rate for the given outcome ranked equal to num.
## If state invalid, throws invalid state error
## If outcome invalid, throws invalid outcome error
## If num invalid, throws invalid num error
## If no value is found corresponding to num, outputs NA

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  names <- names(data)
  valid_outcomes <- c("heart attack" = names[11], "heart failure" = names[17], "pneumonia" = names[23])
  
  ## Check that state and outcome are valid
  if(!(outcome %in% names(valid_outcomes))) {
    
    stop("invalid outcome")
    
  } else if( !(state %in% data[,7])) {
    
    stop("invalid state")
    
  } else if( !is.numeric(num) & num != "best" & num != "worst") {
    stop("invalid num")
  }

  # Partition data frame
  state_data <- data[data[,7] == state,]
  
  # Ensure hospital names can be ordered
  state_data[,2] <- as.character(state_data[,2]) 
  
  # Convert outcome data to numeric and remove NAs
  state_data[,valid_outcomes[outcome]] <- as.numeric(state_data[,valid_outcomes[outcome]])
  state_data <- state_data[!is.na(state_data[,valid_outcomes[outcome]]), ]

  # Orders data by outcome values (low to high), then by hospital name if tied (a - z)
  state_data <- state_data[order(state_data[,valid_outcomes[outcome]], state_data[,2]),]

  if(num == "best") {

    return(state_data[1,2])
    
  } else if(num == "worst") {
    
    return(state_data[length(state_data[,2]),2])
    
  } 

  state_data[num, 2]
  
}