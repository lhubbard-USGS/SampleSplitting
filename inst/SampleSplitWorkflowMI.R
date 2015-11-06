install.packages(c("googleVis"), dependencies=TRUE)
install.packages(c("dataRetrieval"), repos="http://usgs-r.github.com")
install.packages("devtools")
install.packages(c("SampleSplitting"), repos="http://usgs-r.github.com")
# install_github("jlthomps/SampleSplitting")
install.packages(c("USGSwsData", "USGSwsBase", "USGSwsGraphs", "USGSwsStats", "USGSwsQW"), repos="http://usgs-r.github.com")
install.packages(c("XML", "digest", "memoise", "lubridate", "akima", "leaps", "car", "mvtnorm", "relimp", "BSDA", "RODBC"), dependencies=TRUE)
insta########################################################################################################
library(SampleSplitting)
library(dataRetrieval)
library(googleVis)

#EDIT to include the name of the new storm folder
setwd("Q:/crachol/GLRI Edge of Field Study/Sample selection and splits/Alger Creek Basin/2015/MI-TL1/MI-TL1-61/TEST")

# enter NWIS station id for gaging station
siteNo <- "0414826545"
# enter date to begin pulling data (rounded to the day)
StartDt <- '2015-06-07'
# enter date to stop pulling data (rounded to the day)
EndDt <- '2015-06-09'
# enter NWIS station id for precipitation gaging station, may or may not be identical to "siteNo"
precipSite <- "0414826545"
#tzCode <- "America/New_York"

# enter path and name of data file if data is not web-available
dataFile <- "M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/PLAT2TESTJLT.RDB"

# Run ONLY 1 of these options, depending on whether you are pulling data from the web, or providing a file
# Retrieve data from NWISWeb (if available)
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite)
#attr(adaps_data_all$datetime,"tzone") <- "EST"
# or use file names to pull data in from files exported by ADAPS
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite,dataFile)

# save merged data for station/storm event, saved as file, eg 434425090462401data.txt 
adaps_data_allLocal <- adaps_data_all
adaps_data_allLocal$datetime <- adaps_data_allLocal$datetime-(5*60*60)
mergedDataTable(siteNo,StartDt,EndDt,adaps_data_allLocal)

# Generate interactive googleVis plot
hydrographPlot <- hydrographInteractive(adaps_data_allLocal)
plot(hydrographPlot)

# Generate pdf of hydrograph to save, saved as file, eg 434425090462401hydrograph.pdf 
# adjust dateInt as desired to vary length of time (in hours) between x-axis tick marks
hydrographPDF(adaps_data_allLocal,siteNo,dateInt=8)

# IF you have un-sampled storms, you may enter their StormStart and StormEnd values, as well as StormNames in 
# the appropriate list. Leave them out of the maxBottleVol, maxSampVol and subNum lists
# enter the name of the storm(s) (for plot title)
StormName <- c("MITL-61")
# enter Storm Start date(s)
# MUST be in the format YYYY-MM-DD HH:24
StormStart <- c("2015-06-08 03:30")
# enter Storm End date(s) 
# MUST be in the format YYYY-MM-DD HH:24
StormEnd <- c("2015-06-08 16:30")
# enter the maximum possible volume for one sample bottle
maxBottleVol <- c(800)
# enter the maximum possible volume for one full storm sample
maxSampVol <- c(3900)
# enter number for 1st bottle of each storm, if a number other than 1 is desired
subNum <- c(2125)

# generate bottle volume table(s) for lab for each storm
source("C:/Users/jlthomps/Desktop/git/SampleSplitting/R/labDataOutMI.R")
tableOut <- labDataOutMI(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,subNum=subNum)
# look at table(s) generated for lab sample instructions for storm event(s). determine if changes are needed
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}
#Output csv file of all intermediate volumes used for calculations
intermediateVolTable(siteNo,StormStart,StormEnd,tableOut)

# OPTIONAL if sample(s) need to be removed, enter their datetime and a comment and re-create tableOut
# MUST be in the format YYYY-MM-DD HH:24
removeDate <- c("2015-03-10 15:40:00","2015-03-10 16:55:00","2015-03-12 14:10:00","2015-03-12 14:05:00")
removeComment <- c("")
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,removeDate=removeDate,subNum=subNum)
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}
intermediateVolTable(siteNo,StormStart,StormEnd,tableOut)

#Once you are satisfied with the table output
#enter date(s) when samples were picked up 
bottlePickup <- c("Bottles MI-TL1-2125 through MI-TL1-2127 were shipped on 2015-06-08.") 
# generate text file with storm event sample bottle volme table(s)
stormEventsTable(StormName,StormStart,StormEnd,tableOut,maxBottleVol,bottlePickup)

# generate simple table for lab
labVolumesTable(StormName,StormStart,StormEnd,tableOut,bottlePickup)
