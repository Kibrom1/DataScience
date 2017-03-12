##best
best <- function(state, outcome){
  ## Read outcome data from outcome-of-care-measures
  dataold <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that state and outcome are valid
  if(!state %in% dataold$State){
    stop("invalid state")
  }
  else if(!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
  ##capturing the data
  dataold = dataold[dataold$State == state,]
  data = dataold[,c(2, outcomes[outcome])]
  names(data) = c("hospitals", outcome)
  data = data[order(data$hospitals),]
  data = data[order(data[outcome]),]
  return(data[1,1])
}
## Rank hospital in a state function
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  dataold <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that state and outcome are valid
  if(!state %in% dataold$State){
    stop("invalid state")
  }
  else if(!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
  
  dataold = dataold[dataold$State == state,]
  data = dataold[,c(2, outcomes[outcome])]
  names(data) = c("hospitals", outcome)
  data = data[order(data$hospitals),]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data.outcome = data[outcome]
  if(num == "best"){
    num = 1
  }
  else if(num == "worst"){
    num = nrow(data.outcome)-sum(is.na(data.outcome))
  }
  data = data[order(data.outcome),]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(data[num,1])
}
##rank all hospitals
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dataold <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that outcome is valid
  if(!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
  
  if(num == "best"){
    num = 1
  }
  
  data = dataold[,c(2, 7, outcomes[outcome])]
  names(data) = c("hospitals","states", outcome)
  data = data[order(data$hospitals),]
  data = data[order(data[outcome]),]
  data = data[order(data$states),]
  split.data = split(data, data$states)
  #results = sapply(split.data, function(x) x[num,1])
  results = sapply(split.data, function(x){
    if(num == "worst"){
      num = nrow(x[outcome])-sum(is.na(x[outcome]))
    }
    x[num,1]
  })
  
  
  data.frame(hospital=results, state=names(results), row.names=names(results))
}

