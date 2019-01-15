##Rank the hospitals in a given state based on a user-given
##health outcome (ex. "heart attack") and a placement ("best", "worst", or a numeric)
rankhospital <- function(state, outcome, num = "best") {
  #gather the data
  info <- read.csv("outcome-of-care-measures.csv")
  
  #check that the state is valid
  if (!(state %in% info$State)) {
    stop("invalid state")
  }
  
  
  state_data <- split(info, info$State == state)
  state_data <- state_data$"TRUE"
  
  #check for the condition
  #.simpleCap taken from R toupper() documentation
  num_words <- sapply(strsplit(outcome, " "), length)
  c <- NULL
  if (num_words > 1) {
    .simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1, 1)), substring(s, 2),
            sep = "", collapse = " ")
    }
    
    outcome <- .simpleCap(outcome)
    
    c <- paste("Hospital.30.Day.Death..Mortality..Rates.from", 
               strsplit(outcome, " ")[[1]][1], strsplit(outcome, " ")[[1]][2], sep = ".")
  } else {
    outcome <- paste(toupper(substring(outcome, 1, 1)), substring(outcome, 2, nchar(outcome)), sep = "")
    c <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outcome, sep = ".")
  }
  
  if (!(c %in% names(state_data))) {
    stop("invalid outcome")
  }
  
  state_data <- state_data[(state_data[[c]] != "Not Available"), ]
  d <- as.numeric(as.character(state_data[[c]]))
  
  if (num == "best") {
    index <- order(d, state_data$Hospital.Name)[1]
    state_data$Hospital.Name[[index]]
  } else if (num == "worst") {
    index <- order(d, state_data$Hospital.Name)[length(d)]
    state_data$Hospital.Name[[index]]
  } else {
    if (num > length(d)) {
      return(NA)
    }
    
    index <- order(d, state_data$Hospital.Name)[num]
    state_data$Hospital.Name[[index]]
  }
}