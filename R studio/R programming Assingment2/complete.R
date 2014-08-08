complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  dfid<-c()
  nobs<-c()
  for (i in id){
    x<-read.csv(paste(directory,"/",sprintf('%03d',i),".csv",sep=""))
    dfid<-c(dfid,i)
    nobs<-c(nobs,sum(!is.na(x$nitrate*x$sulfate))) 
  }
  id<-dfid
  t<-data.frame(id,nobs)
  t
}