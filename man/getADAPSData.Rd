\name{getADAPSData}
\alias{getADAPSData}
\title{Function to return adaps_data_all df from NWISWeb or previously retrieved RDB files}
\usage{
getADAPSData(siteNo, StartDt, EndDt, dataFile = "")
}
\arguments{
  \item{siteNo}{NWIS gaging station id}

  \item{StartDt}{a date to start data pulls}

  \item{EndDt}{a date to end data pulls}

  \item{dataFile}{string of data file path and name}
}
\value{
adaps_data_all data frame containing merged ADAPS data for
the requested site and date range
}
\description{
This function accepts an NWIS gage site id, an NWIS precip
site id, a StartDate, an EndDate and file names as needed
}
\examples{
\dontrun{
siteNo <- "434425090462401"
StartDt <- '2013-10-03'
EndDt <- '2013-10-05'
getADAPSData(siteNo,StartDt,EndDt)
}
}

