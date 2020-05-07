#'@title calcDemtareaClass
#'@description function obtains monitoring station classification (deciles) based on total drainage area for each site
#'Uses subroutines: errorOccurred. 
#'@param demtarea total drainage area for each site
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return demtarea.class - a decile class labeled with the total drainage area (km2)

calcDemtareaClass <- function(demtarea,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 

      vvar <- demtarea
      iprob<-10
      chk <- unique(quantile(vvar, probs=0:iprob/iprob))
      chk1 <- 11 - length(chk)
      if(chk1 == 0) {
        darea <- quantile(vvar, probs=0:iprob/iprob)
        qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE))
        demtarea.class <- numeric(length(qvars))
        for (k in 1:10) {
          for (i in 1:length(qvars)){
            if(qvars[i] == k) {
              demtarea.class[i] <- round(darea[k+1],digits=0)
            }
          }
        }
      }
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("calcDemtareaClass.R",batch_mode)
      }
    }else{#if no error
       return(demtarea.class)
    }#end if error
    
  }#test if previous error
}#end function
