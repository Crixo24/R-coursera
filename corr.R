corr <- function(directory, threshold=0){
  ids <- 1:332
  correlations <- c()
  
  
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
    aux_s <- c()
    aux_n <- c()
    for (i in 1:length(sulfate)){
      if(!is.na(sulfate[i]) && !is.na(nitrate[i])){
        num_completos <- num_completos + 1
        aux_s <- c(aux_s, sulfate[i])
        aux_n <- c(aux_n, nitrate[i])
      }
    }
      if (num_completos > threshold){
        correlations <- c(correlations, cor(aux_s, aux_n))
      }
    
  }
  
  correlations
}