#'@title calcAccumIncrArea
#'@description function to calculate user selected accumulated incremental area
#'Uses subroutines: accumulateIncrArea, errorOccurred. 
#'@param indata input data (data1 or subdata)
#'@param accumulate_incrementalArea_list character vector of columns to be accumulated (case sensitive)
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return data.frame with accumulated areas



calcAccumIncrArea<-function(indata,accumulate_incrementalArea_list,batch_mode,ErrorOccured){
 
  if (ErrorOccured=="no"){
    tryIt<-try({ 
 
   #get data
  data<-indata
  
  #accumulate incremental area
  areaAccum<-accumulateIncrArea(data,accumulate_incrementalArea_list,paste(accumulate_incrementalArea_list,"_totalArea",sep=""),
                     batch_mode,ErrorOccured)
  
  #merge with data
  data<-merge(data,areaAccum, by = "waterid",all.x=TRUE)
  

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("calcAccumIncrArea.R",batch_mode)
      }
    }else{#if no error
      return(data)
    }#end if error
    
  }#test if previous error
  
  
}#end function