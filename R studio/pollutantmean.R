pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  all<-0
  for (i in id){
    x<-read.csv(paste(directory,"/",sprintf('%03d',i),".csv",sep=""))
    nitrate<-mean(x$nitrate,na.rm=1)
    sulfate<-mean(x$sulfate,na.rm=1)
    polmean<-if (pollutant=="sulfate"){
      sulfate
    }else if (pollutant=="nitrate"){
      nitrate
    }
    all<-c(all,polmean)
   
  }
  print(mean(all))
}