#'@title deleteFiles
#'@description Deletes all files from predict, maps and scenarios directories
#'Subroutine is called when estimation is run.
#'Uses subroutines: errorOccurred.
#'@param path_results path to results directory
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`


deleteFiles<-function(path_results,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  #delete files from prediction, mapping, and scenario folders
  folders<-paste(path_results,c("estimate","predict","maps","scenarios"),"/",sep="")
  for (f in folders){
    filesList<-list.files(f,recursive = TRUE,full.names=TRUE)
    if (length(filesList)!=0){
      msgText<-paste("Deleting files from '",basename(f),"' subdirectory...",sep="")
    message(msgText)
    unlink(filesList,recursive = TRUE)
    }
  }
  

  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("deleteFiles.R",batch_mode)
      }
    }else{#if no error

    }#end if error
    
  }#test if previous error
}#end function