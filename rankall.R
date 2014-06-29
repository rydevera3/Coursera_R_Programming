rankall<-function(outcome,num = "best") {
  
  state_abbrev <- c("AL","AK","AZ","AR","CA",
                    "CO","CT","DE","DC","FL",
                    "GA","HI","ID","IL","IN",
                    "IA","KS","KY","LA","ME",
                    "MT","NE","NV","NH","NJ",
                    "NM","NY","NC","ND","OH",
                    "OK","OR","MD","MA","MI",
                    "MN","MS","MO","PA","RI",
                    "SC","SD","TN","TX","UT",
                    "VT","VA","WA","WV","WI",
                    "WY","VI")
  
  hospital = NULL
  for(k in 1:length(state_abbrev)) {
    hospital[k] <- rankhospital(state_abbrev[k],outcome,num)
  }
  
  sort_s <- sort(state_abbrev,index.return=TRUE)
  df <- data.frame(sort_s$x,hospital[sort_s$ix])
  colnames(df) <- c("state","hospital")
  
  return(df)
  
}