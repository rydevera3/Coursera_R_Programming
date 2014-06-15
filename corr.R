# Find the corrleation between sulfate and nitrate with only the complete observations
# Author: Ryan de Vera
# This function requires the use of the plyr package - install.packages("plyr")

corr <- function(directory, threshold = 0) {
  
  # directory - path where the .csv data can be found
  # threshold - used to determine number of observations over a given threshold
  
  id=1:332
  filenames <- sprintf("%03d.csv",id)
  filenames <- paste(directory,filenames,sep="/")
  
  ldf <- lapply(filenames,read.csv)
  df = ldply(ldf)
  
  nobs<-NULL
  for (k in id) {
    s <- complete.cases(subset(df, ID == k))
    nobs[k] <- length(s[s==TRUE])
  }
  
  ndf <- data.frame(id, nobs)
  
  nrows <- nrow(ndf)
  
  correlation <- NULL
  bool<- ndf[,"nobs"]>threshold
  if(length(bool[bool==FALSE])==nrows) { correlation <- numeric()
  } else {
    for(k in 1:nrows) {
     s<-subset(df,ID==ndf[k,"id"])
     comcases <- complete.cases(s)
     s<-s[comcases==TRUE,]
    if(ndf[k,"nobs"]>threshold) { 
      correlation[k]<-cor(s[,"sulfate"],s[,"nitrate"])
      } else {
       correlation[k]=0 } 
    }
    correlation <- correlation[(correlation!=0 & is.na(correlation)==FALSE)]
  }
  
  return(correlation)
}