complete <- function(directory, ids){
  measures <- c()
  id <- c()
  nobs <- c()

  
  for (f in seq_along(ids)){
    if(floor(ids[f]/10) == 0){
      monitor <- paste("00", toString(ids[f]), ".csv", sep="", collapse=NULL)
    }
    else if(floor(ids[f]/100) == 0){
      monitor <- paste("0", toString(ids[f]), ".csv", sep="", collapse=NULL)
    }
    else{
      monitor <- paste(toString(ids[f]), ".csv", sep="", collapse=NULL)
    }
    #Here ends the process to set up modules' filenames
    
    #getting interesting data
    monitor <- read.csv(file=paste(directory, monitor, sep="", collapse=NULL), head=TRUE, sep=",")
    sulfate <- as.vector(monitor[["sulfate"]])
    nitrate <- as.vector(monitor[["nitrate"]])
    
    num_completos <- 0
    for (i in 1:length(sulfate)){
      if(!is.na(sulfate[i]) && !is.na(nitrate[i])){
        num_completos <- num_completos + 1
      }
    }
    
    id <- c(id, monitor[1, 4])
    nobs <- c(nobs, num_completos)
  }
  
   data.frame(id, nobs)
  
}