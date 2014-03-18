\name{hydrographPDF}
\alias{hydrographPDF}
\title{Function to generate and save pdf version of the hydrograph plot}
\usage{
hydrographPDF(adaps_data_all, storm_name, siteNo)
}
\arguments{
  \item{adaps_data_all}{data frame containing merged ADAPS
  data for the requested storm event}

  \item{storm_name}{string containing the storm name title
  for the plot}

  \item{siteNo}{string containing USGS gage number}
}
\description{
This function accepts a merged data frame of ADAPS data for
a site/storm event
}

