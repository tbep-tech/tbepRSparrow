#'@title createSubdataSorted
#'@description Creates a subset of DATA1, called 'SUBDATA', sorted by HYDSEQ for use in optimization.
#'Selects reaches to be included in the analysis for prediction and calibration
#'Uses subroutines: errorOccurred.
#'@param filter_data1_conditions User specified additional DATA1 variables (and conditions) to be used to filter reaches from sparrow_control
#'@param data1 input data (data1) 
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return subdata - data.frame used in model execution

createSubdataSorted <- function(filter_data1_conditions,data1,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({

  if(is.na(filter_data1_conditions)) {
    data1$fnode[is.na(data1$fnode)] <- 0
    data1$tnode[is.na(data1$tnode)] <- 0
    dname <- paste("subdata <- data1[(data1$fnode > 0 & data1$tnode > 0), ]",sep="") 
    eval(parse(text=dname))   # create subdata            
  } else {
    data1$fnode[is.na(data1$fnode)] <- 0
    data1$tnode[is.na(data1$tnode)] <- 0
    dname1 <- "subdata <- data1[(data1$fnode > 0 & data1$tnode > 0 "
    for (i in 1:length(filter_data1_conditions)) {
      dname1 <- paste(dname1,"& ",filter_data1_conditions[i]," ",sep="") 
    }
    dname <- paste(dname1,"), ]",sep="") 
    eval(parse(text=dname))   # create subdata 
  }

# Sort SUBDATA by HYDSEQ
  subdata <- subdata[with(subdata,order(subdata$hydseq)), ]        # removed secondary sort by waterid (1-8-2017)


    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("createSubdataSorted.R",batch_mode)
      }
    }else{#if no error
        return(subdata)
    }#end if error
    
  }#test if previous error
}#end function
