#######Part 2: Finding the best hospital in a state Sept 12, 2015

best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if(!(state %in% df[, 7])){
    stop("invalid state")
  }
  
  myoutcome <- c("heart attack","heart failure","pneumonia")
  if(!(outcome %in% myoutcome)){
    stop("invalid outcome")
  }
  
  if(outcome=="heart attack"){
    outcome_col <- 11
  }
  else if (outcome=="heart failure"){
    outcome_col <- 17
  }
  else if(outcome=="pneumonia"){
    outcome_col <- 23
  }
  
  subdf <- subset(df, State == state)
  subdf[, outcome_col] <- as.numeric(as.character(subdf[, outcome_col]))
  subdf <- data.frame(subdf[,2],subdf[,outcome_col])
  
  subdf_ord <- subdf[which(subdf[,2] == min(subdf[,2], na.rm = TRUE)),  ]
  
  
  ## Return hospital name in that state with lowest 30-day death rate
  #if there are ties for a state
  if(nrow(subdf_ord) > 1){
    subdf_ord <- subdf_ord[order(subdf_ord[,1]),  ] 
    head(subdf_ord,1)
  }
  else{
    subdf_ord <- head(subdf_ord,2) ##if there are no ties for a state, will print row of na's
    as.character(subdf_ord[1,1])
  }

}