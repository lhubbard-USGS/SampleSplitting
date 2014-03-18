\name{stormEventsTable}
\alias{stormEventsTable}
\title{Function to generate text file with storm event sample bottle volume table(s) and extra info}
\usage{
stormEventsTable(StormName, StormStart, StormEnd, tableOut,
  removeComment = "")
}
\arguments{
  \item{StormName}{vector of storm name(s)}

  \item{StormStart}{vector of storm start dates}

  \item{StormEnd}{vector of storm end dates}

  \item{tableOut}{list of data frames containing event
  data}

  \item{removeComment}{vector of comments}
}
\description{
This function accepts a vector of storm names, vector of
storm start and end datetimes, vector of comments
(optional) and list of dataframes containing event data
}

