startEndmodifySubdata<-function(data_names,class_landuse,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({
 

  #check for missing landuse class
  missingLanduseClass<-class_landuse[which(!class_landuse %in% data_names$sparrowNames)]
  if (length(na.omit(missingLanduseClass))!=0){
    for (i in 1:length(missingLanduseClass)){
      cat("\n FATAL ERROR : MISSING class_landuse : ",missingLanduseClass[i],"\n ",sep="")
      cat("\n \n")
    }
  }
  

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("startEndmodifySubdata.R",batch_mode)
      }
    }else{#if no error
        return(subdata)
    }#end if error
    
  }#test if previous error
}#end function