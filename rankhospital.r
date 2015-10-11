#######Part 3: Ranking hospitals by outcome in a state Sept 14, 2015

rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  subdf <- subset(df, State == state)
  subdf[, outcome_col] <- as.numeric(as.character(subdf[,outcome_col]))
  subdf <- data.frame(subdf[,2],subdf[,outcome_col])
  subdf_ord <- subdf[order(subdf[,2],subdf[,1],na.last = NA),] 
  
  subdf_rank <- data.frame(subdf_ord[,1],subdf_ord[,2])

  if(num=="best"){
    head(subdf_rank,1)
    as.character(head(subdf_rank,1)[1,1]) 
  }
  else if(num=="worst"){
    tail(subdf_rank,1)
    as.character(tail(subdf_rank,1)[1,1])
  }
  else{
    if(num > nrow(subdf_ord)){
      return(NA)
    }
    else{
      subdf_rank <- subdf_rank[num,]
      as.character(subdf_rank[1,1])
    }
  }
  
}