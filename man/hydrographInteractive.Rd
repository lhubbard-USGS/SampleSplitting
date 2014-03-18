\name{hydrographInteractive}
\alias{hydrographInteractive}
\title{Function to return hydrographInteractive html to create interactive hydrograph plot through googleVis}
\usage{
hydrographInteractive(adaps_data_all)
}
\arguments{
  \item{adaps_data_all}{data frame containing merged ADAPS
  data for the requested storm event}
}
\value{
hydrographInteractive html object containing code to create
googleVis plot
}
\description{
This function accepts a merged data frame of ADAPS data for
a site/storm event
}
\examples{
\dontrun{
siteNo <- "434425090462401"
StartDt <- '2013-10-03'
EndDt <- '2013-10-05'
precipSite <- "434425090462401"
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite)
hydrographInteractive(adaps_data_all)
}
}

