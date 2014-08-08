rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
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

  
  data[,outcomeid]<-as.numeric(data[,outcomeid])
    
  data<-data[order(data[,outcomeid],data[,2]),]

  spdata<-split(data,data$State)
  
  if (num=='best'){
    num<-1
  }
  
  if (num=='worst'){
    num<-c()
    for (state in names(spdata)){
    num<-c(num,which.max(spdata[[state]][,outcomeid]))
    }
  }
  
  allstate<-c()
  hospital<-c()
  
  if (length(num)==1){
  for (state in names(spdata)){
      allstate<-c(allstate,state) 
      hospital<-c(hospital,spdata[[state]][num,2])
     }
  }else{
    i=1
    for (state in names(spdata)){
      allstate<-c(allstate,state) 
      hospital<-c(hospital,spdata[[state]][num[i],2])
      i<-i+1
      }
  }
  final<-data.frame(hospital,allstate)
  colnames(final)<-c('hospital','state')
  final
}
