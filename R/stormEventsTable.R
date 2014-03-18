#' Function to generate text file with storm event sample bottle volume table(s) and extra info 
#' 
#' This function accepts a vector of storm names, vector of storm start and end datetimes, vector of comments (optional) and list of dataframes containing event data
#' 
#' @param StormName vector of storm name(s)
#' @param StormStart vector of storm start dates
#' @param StormEnd vector of storm end dates
#' @param tableOut list of data frames containing event data
#' @param maxBottleVol maximum bottle volume
#' @param bottlePickup date bottles were retrieved
#' @param removeComment vector of comments
#' @export
stormEventsTable <- function(StormName,StormStart,StormEnd,tableOut,maxBottleVol,bottlePickup,removeComment=""){
  fileName <- paste(StormName[1],"sampVol",".txt",sep="")
  sink(fileName)
  for (i in 1:length(StormName)) {
    cat(StormName[i],"\t",strftime(StormStart[i]),"\t",strftime(StormEnd[i]),"\n\n")
    print(tableOut[[i]],row.names=FALSE)
    cat("\n\n")
    cat("Lab Sample Volume","\t",sum(tableOut[[i]]$mL),"mL\t",sum(tableOut[[i]]$perc),"percent\n\n")
    cat("Max Bottle Volume","\t",maxBottleVol[i],"mL\n\n")
    cat("Max Optimized Bottle Volue","\t",max(tableOut[[i]]$mL),"mL\n\n")
    cat("Max Sample Runoff Volume","\t",max(tableOut[[i]]$volume),"cubic feet\n\n")
    cat("Total Sampled Storm Volume","\t",sum(tableOut[[i]]$volume),"cubic feet\n\n")
    cat("Bottles ",tableOut[[i]]$subNum[1]," through ",tableOut[[i]]$subNum[length(tableOut[[i]]$subNum)]," picked up ",bottlePickup,"\n\n")
    if (length(removeComment[i])>0) {cat(removeComment[i],"\n\n")}
    cat("========================================================================================================","\n\n")
  }
  sink()
}