library(dataRetrieval)
library(googleVis)
# enter NWIS station id for gaging station
siteNo <- "434425090462401"
# enter date to begin pulling data (rounded to the day)
StartDt <- '2012-06-20'
# enter date to stop pulling data (rounded to the day)
EndDt <- '2012-06-21'
# enter NWIS station id for precipitation gaging station, may or may not be identical to "siteNo"
precipSite <- "434425090462401"
# enter the name of the storm(s) (for plot title)
storm_name <- c("JF6-21")
# enter path and name of data file if data is not web-available
dataFile <- "M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/CASHTONTEST.RDB"

# Retrieve data from NWISWeb (if available), or use file names to pull data in from files exported by ADAPS
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite,dataFile)

# save merged data for station/storm event, saved as file, eg 434425090462401data.txt 
tableOut <- adaps_data_all[,c("agency_cd","site_no","datetime","X01_00065","X02_00060","X05_99234")]
fileName <- paste(siteNo,"data.csv",sep="")
sink(fileName)
cat("Station:"," ",siteNo,"\t","Start date:"," ",strftime(StartDt),"\t","End date:"," ",strftime(EndDt),"\n\n")
write.table(tableOut,file="",sep=",",row.names=FALSE)
sink()

# Generate interactive googleVis plot
hydrographPlot <- hydrographInteractive(adaps_data_all)
plot(hydrographPlot)

# Generate pdf of hydrograph to save, saved as file, eg 434425090462401hydrograph.pdf 
hydrographPDF(adaps_data_all,storm_name,siteNo)

# enter the maximum possible volume for one sample bottle
maxBottleVol <- c(800)
# enter the maximum possible volume for one full storm sample
maxSampVol <- c(3900)
# enter Storm Start date(s)
StormStart <- c(strptime("2012-06-20 20:55","%Y-%m-%d %H:%M"))
#StormStart <- c(strptime("2013-10-03 15:18","%Y-%m-%d %H:%M"),strptime("2013-10-05 2:30","%Y-%m-%d %H:%M"))
# enter Storm End date(s) 
StormEnd <- c(strptime("2012-06-21 05:15","%Y-%m-%d %H:%M"))
#StormEnd <- c(strptime("2013-10-03 21:15","%Y-%m-%d %H:%M"),strptime("2013-10-05 11:30","%Y-%m-%d %H:%M"))
# enter Storm Name(s)
StormName <- c("JF6-21")
#StormName <- c("JF6.38","JF6.39")
# enter number for 1st bottle of each storm, if a number other than 1 is desired
subNum <- c(1)

# generate bottle volume table(s) for lab for each storm
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,subNum=subNum)
removeComment <- c("")
# look at table(s) generated for lab sample instructions for storm event(s). determine if changes are needed
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}

#Output csv file of all intermediate volumes used for calculations
fileName <- paste(siteNo,"SampleVols.csv",sep="")
sink(fileName)
cat("Station:"," ",siteNo,"\t","Start date:"," ",strftime(StartDt),"\t","End date:"," ",strftime(EndDt),"\n\n")
write.table(tableOut[[length(StormStart)+1]],file="",sep=",",row.names=FALSE)
sink()

#Once you are satisfied with the table output
#enter date(s) when samples were picked up 
bottlePickup <- c("2012-06-21")

# if sample(s) need to be removed, enter their datetime and a comment and re-create tableOut
removeDate <- c(strptime("2013-06-01","%Y-%m-%d %H:%M")) #"%Y-%m-%d %H:%M"))
removeComment <- c("")
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,removeDate=removeDate,subNum=subNum)
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}

# generate text file with storm event sample bottle volume table(s)
fileName <- paste(storm_name[1],"sampVol",".txt",sep="")
sink(fileName)
for (i in 1:length(storm_name)) {
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

# generate simple table for lab
fileName <- paste(storm_name[1],"labVolumes",".txt",sep="")
sink(fileName)
for (i in 1:length(storm_name)) {
  cat("==================================================================================","\n")
  cat("\t",StormName[i],"\t\t",strftime(StormStart[i]),"\t",strftime(StormEnd[i]),"\n")
  cat("==================================================================================","\n")
  labTable <- tableOut[[i]]
  for (j in 1:nrow(labTable)) {
  cat("\t",labTable$subNum[j],"\t",strftime(labTable$datetime[j]),"\t",labTable$mL[j],"\n")
  }
  cat("==================================================================================","\n")
  cat("\t","Bottles ",tableOut[[i]]$subNum[1]," through ",tableOut[[i]]$subNum[length(tableOut[[i]]$subNum)]," picked up ",bottlePickup,"\n")
  cat("==================================================================================","\n")
}
sink()

