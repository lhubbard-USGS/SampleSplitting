#' Function to return adaps_lab_samples df derived from previously merged data
#' 
#' This function accepts a data frame of data for a site/storm event, storm start dates, storm end dates, 
#' storm names, maximum volume in one sample bottle, and maximum volume for an entire storm sample
#' 
#' @param adaps_data_all data frame containing merged ADAPS data for the requested site and date range
#' @param StormStart vector of datetimes for storm starts
#' @param StormEnd vector of datetime for storm ends
#' @param StormName vector of storm names
#' @param maxBottleVol vector of maximum volumes in one subsample bottle
#' @param maxSampVol vector of maximum volumes of one total sample
#' @param removeDate vector of datetimes to be removed from the calculation
#' @param subNum vector of starting numbers for first bottle of each storm event
#' @return tableOut list of a table for each storm event of bottle volumes 
#' @import googleVis
#' @export
#' @examples
#' \dontrun{
#' siteNo <- "434425090462401"
#' StartDt <- '2013-10-03'
#' EndDt <- '2013-10-05'
#' adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt)
#' maxBottleVol <- 900
#' maxSampVol <- 3900
#' StormStart <- c(strptime("2013-10-03 15:18","%Y-%m-%d %H:%M"),strptime("2013-10-05 2:30","%Y-%m-%d %H:%M"))
#' StormEnd <- c(strptime("2013-10-03 21:15","%Y-%m-%d %H:%M"),strptime("2013-10-05 11:30","%Y-%m-%d %H:%M"))
#' StormName <- c("JF6-38","JF6-39")
#' labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol)
#' }
labDataOut <- function(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,removeDate="",subNum=-9) {
adaps_data_samples <- adaps_data_all[which(adaps_data_all$X06_99234>0),c("datetime","X02_00060")]
adaps_data_plot <- adaps_data_all[,c("datetime","X01_00065","X02_00060")]

tableOut <- list()
numStorms <- length(StormStart)
for (j in 1:numStorms) {
  StartDt <- StormStart[j]
  EndDt <- StormEnd[j]
  maxBottleV <- maxBottleVol[j]
  maxSampV <- maxSampVol[j]
  row.names(adaps_data_plot)<-(1:nrow(adaps_data_plot))
  startRow <- as.character(as.numeric(row.names(adaps_data_plot[which(StartDt==adaps_data_plot$datetime),]))-1)
  endRow <- as.character(as.numeric(row.names(adaps_data_plot[which(EndDt==adaps_data_plot$datetime),]))+1)
  adaps_data_storm <- adaps_data_plot[startRow:endRow,]
  adaps_data_storm <- adaps_data_storm[which(!is.na(adaps_data_storm$X02_00060)),]
  data_rows <- nrow(adaps_data_storm)
  adaps_data_storm$volume <- 9999
  for (i in 1:data_rows) {
    if (i>1) {
      if (i<data_rows) {
        
        adaps_data_storm$volume[i] <- (.5*(as.numeric(difftime(adaps_data_storm$datetime[i],adaps_data_storm$datetime[i-1],units="secs")))*(.75*adaps_data_storm$X02_00060[i]+.25*adaps_data_storm$X02_00060[i-1]))+(.5*(as.numeric(difftime(adaps_data_storm$datetime[i+1],adaps_data_storm$datetime[i],units="secs")))*(.75*adaps_data_storm$X02_00060[i]+.25*adaps_data_storm$X02_00060[i+1]))
      } else {
        adaps_data_storm$volume[i] <- NA
      }
    } else {adaps_data_storm$volume[i] <- NA}
  }
  adaps_samp_storm <- adaps_data_samples[which(StartDt<=adaps_data_samples$datetime&adaps_data_samples$datetime<=EndDt),]
  if (nrow(adaps_samp_storm)==0) {cat(paste(StormName[j],"Storm event specified which has no samples","\n",sep=" "))}
  if (subNum[1]>0) {subStart<-subNum[j]} else {subStart<-1}
  subMax <- nrow(adaps_samp_storm)+(subStart-1)
  adaps_samp_storm$subNum <- c(subStart:subMax)
  adaps_samp_storm$subNum <- paste(StormName[j],adaps_samp_storm$subNum,sep="-")
  if (length(removeDate)>0) {
    for (i in 1:length(removeDate)) {
      adaps_samp_storm <- adaps_samp_storm[which(adaps_samp_storm$datetime!=removeDate[i]),]
    }}
  adaps_samp_storm$volume <- 9999
  adaps_samp_storm$sampStar <- StartDt
  adaps_samp_storm$sampEnd <- EndDt
  samplesNum <- nrow(adaps_samp_storm)
  for (i in 1:samplesNum) {
    sampStart <- if (i>1) {adaps_samp_storm$datetime[i-1]+(.5*(adaps_samp_storm$datetime[i]-adaps_samp_storm$datetime[i-1]))} else {min(adaps_data_storm$datetime)}
    sampEnd <- if (i<samplesNum) {adaps_samp_storm$datetime[i]+(.5*(adaps_samp_storm$datetime[i+1]-adaps_samp_storm$datetime[i]))} else {max(adaps_data_storm$datetime)}
    adaps_data_storm_temp <- adaps_data_storm[which(adaps_data_storm$datetime>=sampStart&adaps_data_storm$datetime<sampEnd),]
    if (nrow(adaps_data_storm[which(adaps_data_storm$datetime==sampEnd),])==0) {
      adaps_data_storm_temp_end <- adaps_data_storm_temp[1,]
      adaps_data_storm_temp_end$datetime <- min(adaps_data_storm$datetime[which(adaps_data_storm$datetime>sampEnd)])
      adaps_data_storm_temp_end$X01_00065 <- (adaps_data_storm$X01_00065[which(adaps_data_storm$datetime==adaps_data_storm_temp_end$datetime)])
      adaps_data_storm_temp_end$X02_00060 <- (adaps_data_storm$X02_00060[which(adaps_data_storm$datetime==adaps_data_storm_temp_end$datetime)])
      adaps_data_storm_temp_end$volume <- 0.5*(adaps_data_storm$volume[which(adaps_data_storm$datetime==adaps_data_storm_temp_end$datetime)])
      adaps_data_storm_temp <- rbind(adaps_data_storm_temp,adaps_data_storm_temp_end)
    }
    if (nrow(adaps_data_storm[which(adaps_data_storm$datetime==sampStart),])==0) {
      adaps_data_storm_temp$volume[1] <- 0.5*(adaps_data_storm_temp$volume[1])
    }
      
    adaps_samp_storm$volume[i] <- sum(adaps_data_storm_temp$volume,na.rm=TRUE)
    adaps_samp_storm$sampStar[i] <- sampStart
    adaps_samp_storm$sampEnd[i] <- sampEnd
    adaps_data_storm_temp$samplesNum <- rep(adaps_samp_storm$subNum[i],nrow(adaps_data_storm_temp))
    adaps_data_samp <- if (i+j>2) {rbind(adaps_data_samp,adaps_data_storm_temp)} else {adaps_data_storm_temp}
  }
  adaps_data_samp <- subset(adaps_data_samp, !is.na(adaps_data_samp$volume))
  adaps_samp_storm <- subset(adaps_samp_storm, !is.na(adaps_samp_storm$volume))
  adaps_samp_storm$perc <- round(100*(adaps_samp_storm$volume/sum(adaps_data_storm$volume,na.rm=TRUE)),digits=1)
  adaps_samp_storm$mL <- adaps_samp_storm$volume*maxBottleV/max(adaps_samp_storm$volume,na.rm=TRUE)
  if (sum(adaps_samp_storm$mL,na.rm=TRUE)>maxSampV) {
    currSum <- sum(adaps_samp_storm$mL,na.rm=TRUE)
    adaps_samp_storm$mL <- trunc(adaps_samp_storm$mL*(maxSampV/currSum))
  } else { adaps_samp_storm$mL <- trunc(adaps_samp_storm$mL)}
  tableOut[[j]] <- adaps_samp_storm[,c("subNum","datetime","mL","perc","volume","sampStar","sampEnd")]
}
tableOut[[j+1]] <- adaps_data_samp
return(tableOut)
}
