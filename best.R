best <- function(state,outcome) {
  library("Hmisc")
  hosp_results<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  state_abbrev = c("AL","AK","AZ","AR","CA",
                   "CO","CT","DE","DC","FL",
                   "GA","HI","ID","IL","IN",
                   "IA","KS","KY","LA","ME",
                   "MT","NE","NV","NH","NJ",
                   "NM","NY","NC","ND","OH",
                   "OK","OR","MD","MA","MI",
                   "MN","MS","MO","PA","RI",
                   "SC","SD","TN","TX","UT",
                   "VT","VA","WA","WV","WI",
                   "WY")
  outcome_list = c("heart attack","heart failure","pneumonia")
  
  if(state %nin% state_abbrev) {stop("invalid state")}
  if(outcome %nin% outcome_list) {stop("invalid outcome")}
  
  state_outcome = subset(hosp_results,hosp_results[,7]==state)
  
  if(outcome==outcome_list[1]) {
    s<-as.numeric(state_outcome[,11])
  }else if(outcome==outcome_list[2]) {
    s<-as.numeric(state_outcome[,17])
  }else {
    s<-as.numeric(state_outcome[,23])
  }
  options(warn=-1)
  
  m = min(s,na.rm=TRUE)
  idx = which(s==min(s,na.rm=TRUE))
  
  result = sort(state_outcome[idx,2])
  
  return(result[1])
  
}