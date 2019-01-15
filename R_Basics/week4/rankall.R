source("rankhospital.R")

##Ranks hospitals in every state and returns a data frame
##containing the ranked hospitals and their corresponding states
rankall <- function(outcome, num = "best") {
  info <- read.csv("outcome-of-care-measures.csv")
  hnames <- lapply(levels(info$State), rankhospital, outcome = outcome, num = num)
  ##hnames is a list, we must make it a character vector to fit in the data frame
  data.frame(hospital = as.character(unlist(hnames)), state = levels(info$State))
}