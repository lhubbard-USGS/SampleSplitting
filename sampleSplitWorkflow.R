library(sampleSplitting)
# enter NWIS station id for gaging station
siteNo <- "434425090462401"
# enter date to begin pulling data (rounded to the day)
StartDt <- '2012-06-20'
# enter date to stop pulling data (rounded to the day)
EndDt <- '2012-06-21'
# enter NWIS station id for precipitation gaging station, may or may not be identical to "siteNo"
precipSite <- "434425090462401"
# enter the name of the storm(s) (for plot title)
StormName <- c("JF6-21")
# enter path and name of data file if data is not web-available
dataFile <- "M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/CASHTONTEST.RDB"

# Retrieve data from NWISWeb (if available), or use file names to pull data in from files exported by ADAPS
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite,dataFile)

# save merged data for station/storm event, saved as file, eg 434425090462401data.txt 
mergedDataTable(siteNo,StartDt,EndDt,adaps_data_all)

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
# enter number for 1st bottle of each storm, if a number other than 1 is desired
subNum <- c(1)

# generate bottle volume table(s) for lab for each storm
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,subNum=subNum)
# look at table(s) generated for lab sample instructions for storm event(s). determine if changes are needed
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}

#Output csv file of all intermediate volumes used for calculations
intermediateVolTable(siteNo,StormStart,StormEnd,tableOut)

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
stormEventsTable(StormName,StormStart,StormEnd,tableOut,maxBottleVol,bottlePickup)

# generate simple table for lab
labVolumesTable(StormName,StormStart,StormEnd,tableOut,bottlePickup)

