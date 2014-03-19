#' Function to csv file of all intermediate volumes used for calculations 
#' 
#' This function accepts a siteNo, StartDt, EndDt and tableOut list of data for a given storm event
#' 
#' @param siteNo USGS station id
#' @param StartDt start of storm event
#' @param EndDt end of storm event
#' @param tableOut list of data frames of event data
#' @export
intermediateVolTable <- function(siteNo,StartDt,EndDt,tableOut){
  fileName <- paste(siteNo,"SampleVols.csv",sep="")
  sink(fileName)
  cat("Station:"," ",siteNo,"\t","Start date:"," ",strftime(StartDt),"\t","End date:"," ",strftime(EndDt),"\n\n")
  write.table(tableOut[[length(tableOut)]],file="",sep=",",row.names=FALSE)
  sink()
}