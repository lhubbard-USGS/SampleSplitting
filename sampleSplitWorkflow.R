library(sampleSplitting)
library(dataRetrieval)
library(googleVis)
# enter NWIS station id for gaging station
siteNo <- "424314090240601"
# enter date to begin pulling data (rounded to the day)
StartDt <- '2008-05-30'
# enter date to stop pulling data (rounded to the day)
EndDt <- '2008-06-15'
# enter NWIS station id for precipitation gaging station, may or may not be identical to "siteNo"
precipSite <- "434425090462401"
# enter path and name of data file if data is not web-available
dataFile <- "M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/PLAT2TEST.RDB"

# Retrieve data from NWISWeb (if available)
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite)
# or use file names to pull data in from files exported by ADAPS
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite,dataFile)

# save merged data for station/storm event, saved as file, eg 434425090462401data.txt 
mergedDataTable(siteNo,StartDt,EndDt,adaps_data_all)

# Generate interactive googleVis plot
hydrographPlot <- hydrographInteractive(adaps_data_all)
plot(hydrographPlot)

# Generate pdf of hydrograph to save, saved as file, eg 434425090462401hydrograph.pdf 
hydrographPDF(adaps_data_all,"S2",siteNo)

# enter the maximum possible volume for one sample bottle
maxBottleVol <- c(400,600,600,600,600,600,600,400,600,800)
# enter the maximum possible volume for one full storm sample
maxSampVol <- c(3900,3900,3900,3900,3900,3900,3900,3900,3900,3900)
# enter Storm Start date(s)
StormStart <- c(strptime("2008-05-30 02:51","%Y-%m-%d %H:%M"),strptime("2008-06-05 04:39","%Y-%m-%d %H:%M"),
                strptime("2008-06-06 04:22","%Y-%m-%d %H:%M"),strptime("2008-06-07 22:52","%Y-%m-%d %H:%M"),
                strptime("2008-06-08 08:41","%Y-%m-%d %H:%M"),strptime("2008-06-08 19:03","%Y-%m-%d %H:%M"),
                strptime("2008-06-12 09:03","%Y-%m-%d %H:%M"),strptime("2008-06-12 21:40","%Y-%m-%d %H:%M"),
                strptime("2008-06-14 16:52","%Y-%m-%d %H:%M"),strptime("2008-06-15 04:07","%Y-%m-%d %H:%M"))
#StormStart <- c(strptime("2013-10-03 15:18","%Y-%m-%d %H:%M"),strptime("2013-10-05 2:30","%Y-%m-%d %H:%M"))
# enter Storm End date(s) 
StormEnd <- c(strptime("2008-05-30 08:49","%Y-%m-%d %H:%M"),strptime("2008-06-05 07:21","%Y-%m-%d %H:%M"),
              strptime("2008-06-06 05:28","%Y-%m-%d %H:%M"),strptime("2008-06-08 01:14","%Y-%m-%d %H:%M"),
              strptime("2008-06-08 11:39","%Y-%m-%d %H:%M"),strptime("2008-06-08 21:31","%Y-%m-%d %H:%M"),
              strptime("2008-06-12 10:22","%Y-%m-%d %H:%M"),strptime("2008-06-13 01:36","%Y-%m-%d %H:%M"),
              strptime("2008-06-14 18:05","%Y-%m-%d %H:%M"),strptime("2008-06-15 09:22","%Y-%m-%d %H:%M"))
#StormEnd <- c(strptime("2013-10-03 21:15","%Y-%m-%d %H:%M"),strptime("2013-10-05 11:30","%Y-%m-%d %H:%M"))
# enter the name of the storm(s) (for plot title)
StormName <- c("S2-066","S2-067","S2-068","S2-069","S2-070","S2-071","S2-072","S2-073","S2-074","S2-075")
# enter number for 1st bottle of each storm, if a number other than 1 is desired
subNum <- c(1,1,1,1,16,1,1,5,1,7)

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
removeDate <- c(strptime("2008-05-30 07:44","%Y-%m-%d %H:%M")) #"%Y-%m-%d %H:%M"))
removeComment <- c("")
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,removeDate=removeDate,subNum=subNum)
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}
intermediateVolTable(siteNo,StormStart,StormEnd,tableOut)

# generate text file with storm event sample bottle volume table(s)
stormEventsTable(StormName,StormStart,StormEnd,tableOut,maxBottleVol,bottlePickup)

# generate simple table for lab
labVolumesTable(StormName,StormStart,StormEnd,tableOut,bottlePickup)

