#'@title fixDupLatLons
#'@description Adds a small random increment of decimal degrees to the latitude and longitude 
#'            for monitoring sites with duplicate values. \\cr \\cr
#'Executed By: diagnosticSpatialAutoCorr.R \\cr
#'@param latLon numeric vector of latitude or longitude values
#'@return `x` latitude or longitude vector with small random increment added to duplicate 
#'            latitudes and longitudes



fixDupLatLons <- function(latLon){
  
  
  xd <- duplicated(latLon)
  for (i in 1:length(xd)) {
    if(xd[i]==TRUE) {
      latLon[i] <- latLon[i]+runif(1,min=0.000001,max=0.000009)
    }
  }
  
  return(latLon)
  
}#end function

