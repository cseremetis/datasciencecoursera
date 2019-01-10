##A function that takes a directory containing environmental data
##and an interval representing the number of files to scan
##scans the files and returns a dataframe that tells the number of
##complete observations per environmental indicator per file
complete <- function(directory, id = 1:332) {
  p <- paste(getwd(), '/', directory, sep = '') #store path in var p
  files <- list.files(path = p) #dump all the files in a vector
  
  ids <- c(NULL)
  cases <- c(NULL)
  
  for (i in id) {
    x <- read.csv(paste(p, '/', files[i], sep = '')) ##read the file
    n <- length(x[complete.cases(x["nitrate"]), "nitrate"]) #num of complete cases of nitrate
    s <- length(x[complete.cases(x["sulfate"]), "sulfate"]) #num of complete cases of sulfate
    tot <- min(n, s) #the minimum of n and s is our result
    cases <- c(cases, tot)
    ids <- c(ids, i)
  }
  
  data.frame(id = ids, nobs = cases)
}