#
# findMinMaxLatLon.R
#
##############################################

findMinMaxLatLon <- function(sitedata,lat_limit,lon_limit,mapping.input.list,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  lat <- ifelse(sitedata$lat == 0,NA,sitedata$lat)
  lon <- ifelse(sitedata$lon == 0,NA,sitedata$lon)
  latmax <- max(lat,na.rm=TRUE)
  latmin <- min(lat,na.rm=TRUE)
  lonmax <- max(lon,na.rm=TRUE)
  lonmin <- min(lon,na.rm=TRUE)
  sitegeolimits <- named.list(latmax,latmin,lonmax,lonmin)
 
  if (is.na(lat_limit)){
    mapping.input.list$lat_limit<-c(latmin-2,latmax+2)
assign("mapping.input.list",mapping.input.list,envir = .GlobalEnv)
  assign("lat_limit",c(latmin-2,latmax+2),envir = .GlobalEnv)
  }
  if (is.na(lon_limit)){
    mapping.input.list$lon_limit<-c(lonmin-2,lonmax+2)
    assign("mapping.input.list",mapping.input.list,envir = .GlobalEnv)
  assign("lon_limit",c(lonmin-2,lonmax+2),envir = .GlobalEnv)
  }
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("findMinMaxLatLon.R",batch_mode)
      }
    }else{#if no error
      return(sitegeolimits)
    }#end if error
    
  }#test if previous error
}#end function

