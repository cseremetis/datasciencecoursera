##Given a directory containing CSV files, a pollutant to search for
##and an interval, find the mean of that pollutant's data
pollutantmean <- function(directory, pollutant, id = 1:332) {
  p <- paste(getwd(), '/', directory, sep = '')
  files <- list.files(path = p) #dump all the files in a vector
  data <- c(NULL) #we're gonna use this to collect all the columns in one place
  
  for (i in id) {
    x <- read.csv(paste(p, '/', files[i], sep = '')) #read the next file
    x <- x[complete.cases(x[pollutant]), pollutant] #remove NAs
    
    if (length(x) != 0) {
      data <- c(data, x) #build our data vector
    }
  }
  
  mean(data)
}