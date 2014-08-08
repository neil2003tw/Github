corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  all<-c()
  t<-complete("specdata")
  for (i in 1:332){
    x<-read.csv(paste(directory,"/",sprintf('%03d',i),".csv",sep=""))
    if (t$nobs[[i]]>threshold){
      all[i]=cor(x$sulfate,x$nitrate,use="na.or.complete")
    } 
  }
  print(all)
}