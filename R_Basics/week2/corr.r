corr <- function(directory, threshold = 0) {
  p <- paste(getwd(), '/', directory, sep = '') #store path in var p
  files <- list.files(path = p) #dump all the files in a vector
  correlations <- c(NULL)
  
  for (i in 1:332) {
    x <- read.csv(paste(p, '/', files[i], sep = '')) #read the file
    n <- length(x[complete.cases(x["nitrate"]), "nitrate"]) #num of complete cases of nitrate
    s <- length(x[complete.cases(x["sulfate"]), "sulfate"]) #num of complete cases of sulfate
    
    if (min(n, s) > threshold) {
      correlations <- c(correlations, cor(x["nitrate"], x["sulfate"], use = "pairwise.complete.obs"))
    }
  }
  
  return(correlations)
}