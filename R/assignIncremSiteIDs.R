#'@title assignIncremSiteIDs
#'@description Assign site IDs to incremental areas by climbing network structure (reaches sorted by HYDSEQ)
#'Uses subroutines: errorOccurred. 
#'Used By subroutines : calcClassLandusePercent
#'@param minnum user selected minimum number of reaches between sites
#'@param staid reach site IDs (non-zero for reaches with selected monitoring sites)
#'@param waterid original site ids from data1
#'@param tnode 'to node' from data1
#'@param fnode 'from node' from data1
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return rchstaid vector site IDs assigned contiguously to upstream incremental reaches

 assignIncremSiteIDs <- function(minnum,staid,waterid,tnode,fnode,batch_mode,ErrorOccured) { 
if (ErrorOccured=="no"){
   tryIt<-try({   

    numrchs <- length(tnode)
    nnode <- max(tnode,fnode)
    rchstaid <- matrix(0,nrow=numrchs,ncol=1)

    return_data <- .Fortran('sites_incr',
     numrchs=as.integer(numrchs),
     nnode=as.integer(nnode),
     minnum=as.integer(minnum),
     fnode=as.integer(fnode),
     tnode=as.integer(tnode),
     staid=as.integer(staid),
     waterid=as.integer(waterid),
     rchstaid=as.integer(rchstaid),PACKAGE='sites_incr')
     rchstaid <- return_data$rchstaid

 },TRUE)#end try
   
if (class(tryIt)=="try-error"){#if an error occured
  if(ErrorOccured=="no"){
    errorOccurred("assignIncremSiteIDs.R",batch_mode)
  }
}else{#if no error
  return(rchstaid)   
  }#end if error

   }#test if previous error
 }#end function