outputSettings<-function(path_results,file_sum,csv_decimalSeparator, csv_columnSeparator,save,
                         batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

    #get all settings
  settings<-c(getCharSett(),getNumSett(),getShortSett(),getYesNoSett())
  
  #format options settings
  optSettings<-getOptionSett()
  optSettings<-unlist(lapply(optSettings,function(x) trimws(strsplit(x,"=")[[1]][1])))
  settings<-c(settings,optSettings)
  
  #make dataframe and add values for settings
  settings<-data.frame(setting = settings)
  settings$value<-NA
  for (s in settings$setting){
  settings[which(settings$setting==s),]$value<-paste(capture.output(dput(get(s))),collapse=", ")
  }

  if (save==TRUE){
  #output to csv
  fwrite(settings, file=paste(path_results,file_sum,"_userSettings.csv",sep=""),
         showProgress = FALSE,row.names=FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,
         col.names = TRUE,na = "NA")
  }
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("outputSettings.R",batch_mode)
      }
    }else{#if no error
      return(settings)
    }#end if error
    
  }#test if previous error
}#end function