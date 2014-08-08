rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data<-read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  
  outcomeid<-if(outcome=="heart attack"){
    11
  }else if(outcome=="heart failure"){
    17
  }else if(outcome=="pneumonia"){
    23
  }else{
    stop("invalid outcome")
  }
  
  
  data<-data[data$State==state,]
  if(nrow(data)==0){
    stop('invalid state')
  }
  
  
  data[,outcomeid]<-as.numeric(data[,outcomeid])
  
  
  orderedhospital<-data[order(data[,outcomeid]),2]
  
  if (num=='best'){
    num=1
  }else if(num=='worst'){
    num=which.max(data[order(data[,outcomeid]),outcomeid])
  }
  
  orderedhospital[num]
  
}
