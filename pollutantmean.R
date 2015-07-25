pollutantmean <- function(directory, pollutant, id=1:332){
  measures <- c()

  for (f in seq_along(id)){
    if(floor(id[f]/10) == 0){
      monitor <- paste("00", toString(id[f]), ".csv", sep="", collapse=NULL)
    }
    else if(floor(id[f]/100) == 0){
      monitor <- paste("0", toString(id[f]), ".csv", sep="", collapse=NULL)
    }
    else{
      monitor <- paste(toString(id[f]), ".csv", sep="", collapse=NULL)
    }
    #Here ends the process to set up modules' filenames
    
    #Isolating data from NA
    monitor <- read.csv(file=paste(directory, monitor, sep="", collapse=NULL), head=TRUE, sep=",")
    monitor <- monitor[pollutant]
    na <- is.na(monitor)
    monitor <- monitor[!na]
    
    #Once clean data are isolated, they are stored into a common vector.
    measures <- c(measures, monitor)
  }
  
  mean(measures)
}