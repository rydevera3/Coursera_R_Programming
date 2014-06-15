# Find the mean of a pollutant in a data set
# Author: Ryan de Vera
# The use of the function requires the plyr package - install.packages("plyr")


pollutantmean <- function(directory, pollutant, id=1:332) {
  
  # directory - the directory where the .csv files can be found
  # pollutant - "sulfate" or "nitrate"
  # id - ex: 1:332, c(2,4,8,10,12)
  
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory,filenames,sep="/")
  
  ldf <- lapply(filenames,read.csv)
  df = ldply(ldf)
  
  return(mean(df[,pollutant], na.rm=TRUE))
  
  
}