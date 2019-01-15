##Find the best hospital in a given state to treat patients with a given condition
best <- function(state, outcome) {
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
  index <- order(as.numeric(as.character(unlist(state_data[c]))), state_data$Hospital.Name)[1]
  state_data$Hospital.Name[index]
}