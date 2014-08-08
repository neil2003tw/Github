best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  data<-read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

  
  outcomeid<-if(outcome=="heart attack"){
    11
  }else if(outcome=="heart failure"){
    17
  }else if(outcome=="pneumonia"){
    23
  }else{
    stop("Please give valid disease name")
  }
  
  
  data<-data[data$State==state,]
  if(nrow(data)==0){
    stop('invalid state')
  }
  data[,outcomeid]<-as.numeric(data[,outcomeid])
  data[which.min(data[,outcomeid]),2]
  
  
}
