#' Function to return data availability, internal or public, from NWISWeb
#' 
#' This function accepts an NWIS gage site id
#' 
#' @param siteNo NWIS gaging station id
#' @return SiteFile data frame of data available for a given site from NWISWeb
#' @import dataRetrieval
#' @export
#' @examples
#' \dontrun{
#' siteNo <- "441520088045002"
#' StartDt <- '2014-03-10'
#' EndDt <- '2014-03-17'
#' paramAvailability(siteNo)
#' }
paramAvailability <- function (siteNo) {
  urlSitefile3 <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=", 
                       siteNo, "&access=3",sep = "")
  SiteFile3 <- read.delim(urlSitefile3, header = TRUE, quote = "\"", 
                         dec = ".", sep = "\t", colClasses = c("character"), fill = TRUE, 
                         comment.char = "#",strip.white=TRUE)
  SiteFile3 <- SiteFile3[-1, ]
  SiteFile3 <- with(SiteFile3, data.frame(parameter_cd = parm_cd, 
                                        statCd = stat_cd, startDate = begin_date, endDate = end_date, 
                                        count = count_nu, service = data_type_cd, access = 3, stringsAsFactors = FALSE))
  urlSitefile2 <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=", 
                        siteNo, "&access=2",sep = "")
  SiteFile2 <- read.delim(urlSitefile2, header = TRUE, quote = "\"", 
                          dec = ".", sep = "\t", colClasses = c("character"), fill = TRUE, 
                          comment.char = "#",strip.white=TRUE)
  SiteFile2 <- SiteFile2[-1, ]
  SiteFile2 <- with(SiteFile2, data.frame(parameter_cd = parm_cd, 
                                         statCd = stat_cd, startDate = begin_date, endDate = end_date, 
                                         count = count_nu, service = data_type_cd, access = 2, stringsAsFactors = FALSE))
  urlSitefile1 <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=", 
                        siteNo, "&access=1",sep = "")
  SiteFile1 <- read.delim(urlSitefile1, header = TRUE, quote = "\"", 
                          dec = ".", sep = "\t", colClasses = c("character"), fill = TRUE, 
                          comment.char = "#",strip.white=TRUE)
  SiteFile1 <- SiteFile1[-1, ]
  SiteFile1 <- with(SiteFile1, data.frame(parameter_cd = parm_cd, 
                                          statCd = stat_cd, startDate = begin_date, endDate = end_date, 
                                          count = count_nu, service = data_type_cd, access = 1, stringsAsFactors = FALSE))
  SiteFile <- rbind(SiteFile3,SiteFile2,SiteFile1)
  SiteFile <- SiteFile[!is.na(SiteFile$parameter_cd), ]
  SiteFile <- SiteFile["" != SiteFile$parameter_cd, ]
  access_cd <- aggregate(SiteFile$access,by=list(SiteFile$parameter_cd,SiteFile$statCd,SiteFile$service),max)
  colnames(access_cd) <- c("parameter_cd","statCd","service","status")
  SiteFile <- SiteFile[,c("parameter_cd","statCd","startDate","endDate","count","service")]
  SiteFile <- unique(SiteFile)
  SiteFile$startDate <- as.Date(SiteFile$startDate)
  SiteFile$endDate <- as.Date(SiteFile$endDate)
  SiteFile$count <- as.numeric(SiteFile$count)
  SiteFile <- merge(SiteFile,access_cd,by=c("parameter_cd","statCd","service"))
  pCodes <- unique(SiteFile$parameter_cd)
  pcodeINFO <- readNWISpCode(pCodes)
  SiteFile <- merge(SiteFile, pcodeINFO, by = "parameter_cd")
  return(SiteFile)
}