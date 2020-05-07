# fixDupLatLons.R
#
# Adds small random increment to duplicate latitudes and longitudes
#
#####################################################################
fixDupLatLons <- function(x,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 
 
  xd <- duplicated(x)
  for (i in 1:length(xd)) {
    if(xd[i]==TRUE) {
      x[i] <- x[i]+runif(1,min=0.000001,max=0.000009)
    }
  }
 
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("fixDupLatLons.R",batch_mode)
      }
    }else{#if no error
 return(x)
    }#end if error
    
  }#test if previous error
}#end function

