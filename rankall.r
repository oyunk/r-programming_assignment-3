#######Part 4: Ranking hospitals in all states Sept 15-Oct 8, 2015

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome and num are valid
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
  
  ## For each state, find the hospital of the given rank
  uniqState <- unique(df[, 7])
  uniqState <- as.character(uniqState)
  
  subdfList <- list()  ## create empty list
  
  #for each unique state, get subset, convert NAs into numeric, create new dfs with hospital, state and rank,
  # and reorder by rate then hospital name, then get only specified rank for subset 
  for(i in seq_along(uniqState)) { 
    
    subdf <- subset(df, State == uniqState[i])
    subdf[, outcome_col] <- as.numeric(as.character(subdf[,outcome_col]))
    subdf <- data.frame(subdf[,2],subdf[,7],subdf[,outcome_col])
    subdf_ord <- subdf[order(subdf[,3],subdf[,1],na.last = NA),]
    
    subdf_rank <- subdf_ord[num,]
    
    if(num=="best"){
      subdf_rank <- head(subdf_ord,1)
    }
    else if(num=="worst"){
      subdf_rank <- tail(subdf_ord,1)
    }
    
    subdf_rank[,2] <- uniqState[i]  ## rename all state col values (incl those with NAs) to the state
    subdf_rank
    row.names(subdf_rank) <- uniqState[i]  #rename row as the state
    
    subdf_rank[,3] <- NULL #drop column 3 (rank), after ordering from above
    
    subdfList[[i]] <- subdf_rank
    
  }
  ## Return a data frame with the hospital names and the (abbreviated) state name
  
  df_final<-do.call("rbind", subdfList)
  colnames(df_final) <- c("hospital","state")
  df_final
  
  df_final_ord <- df_final[order(df_final[,2],na.last = NA),] #reorder final df by state
  df_final_ord
  
}
