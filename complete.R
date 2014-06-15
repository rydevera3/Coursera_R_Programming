# Find the complete number of observations in a data set
# Author: Ryan de Vera
# This function requires the use of the plyr package - install.packages("plyr")

complete <- function(directory,id) {
  
  # directory - path where the .csv data can be found
  # id - ex: 1:332, c(2,3,8,10,12)
  
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory,filenames,sep="/")
  
  ldf <- lapply(filenames,read.csv)
  df = ldply(ldf)
  
  nobs<-NULL
  for (k in id) {
  s <- complete.cases(subset(df, ID == k))
  nobs[k] <- length(s[s==TRUE])
   }
  
  nobs <- nobs[id]
  
  ndf <- data.frame(id, nobs)
  return(ndf)
  
}