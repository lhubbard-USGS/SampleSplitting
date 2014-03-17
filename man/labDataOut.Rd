\name{labDataOut}
\alias{labDataOut}
\title{Function to return adaps_lab_samples df derived from previously merged data}
\usage{
labDataOut(adaps_data_all, StormStart, StormEnd, StormName, maxBottleVol,
  maxSampVol, removeDate = "", subNum = -9)
}
\arguments{
  \item{adaps_data_all}{data frame containing merged ADAPS
  data for the requested site and date range}

  \item{StormStart}{vector of datetimes for storm starts}

  \item{StormEnd}{vector of datetime for storm ends}

  \item{StormName}{vector of storm names}

  \item{maxBottleVol}{vector of maximum volumes in one
  subsample bottle}

  \item{maxSampVol}{vector of maximum volumes of one total
  sample}

  \item{removeDate}{vector of datetimes to be removed from
  the calculation}

  \item{subNum}{vector of starting numbers for first bottle
  of each storm event}
}
\value{
tableOut list of a table for each storm event of bottle
volumes
}
\description{
This function accepts a data frame of data for a site/storm
event, storm start dates, storm end dates, storm names,
maximum volume in one sample bottle, and maximum volume for
an entire storm sample
}
\examples{
\dontrun{
siteNo <- "434425090462401"
StartDt <- '2013-10-03'
EndDt <- '2013-10-05'
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt)
maxBottleVol <- 900
maxSampVol <- 3900
StormStart <- c(strptime("2013-10-03 15:18","\%Y-\%m-\%d \%H:\%M"),strptime("2013-10-05 2:30","\%Y-\%m-\%d \%H:\%M"))
StormEnd <- c(strptime("2013-10-03 21:15","\%Y-\%m-\%d \%H:\%M"),strptime("2013-10-05 11:30","\%Y-\%m-\%d \%H:\%M"))
StormName <- c("JF6-38","JF6-39")
labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol)
}
}

