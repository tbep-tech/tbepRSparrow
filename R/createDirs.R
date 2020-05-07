#'@title createDirs
#'@description Function to creates model subdirectory name specified by run_id setting in sparrow_control.  Copies control files from results directory into model subdirectory.
#'Uses subroutines: errorOccurred.
#'@param oldName activeFile - path to current sparrow_control file
#'@param newName run_id setting from sparrow_control file
#'@param path_results path to results directory
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param if_userModifyData yes/no character string indicating whether or not userModifyData.R is to be run.
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return TRUE/FALSE logical indicating whether or not function ran successfully


createDirs<-function(oldName,newName,pathResults,batch_mode,if_userModifyData,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({
 
  options(warn=-1)
  pathResults<-dirname(pathResults)
  #create main directory
  dir.create(paste(pathResults,"/",newName,sep=""))
  #createsubdirectories
  dirList<-c("data",
             "estimate",
             "maps",
             "predict",
             "scenarios")

 sapply(dirList, function(x) dir.create(paste(pathResults,"/",newName,"/",x,sep="")))
  if (batch_mode=="yes"){
    dir.create(paste(pathResults,"/",newName,"/","batchSessionInfo",sep=""))
  }
  #save control files
  filesList<-c("sparrow_control.R",
               "parameters.csv",
               "design_matrix.csv",
               "userModifyData.R",
               "dataDictionary.csv")
  if (if_userModifyData=="no"){
    filesList<-filesList[which(filesList!="userModifyData.R")]
  }
    fileCopy<-sapply(filesList, function(x) file.copy(paste(pathResults,"/",x,sep=""),
                                            paste(pathResults,"/",newName,"/",newName,"_",x,sep=""),overwrite=TRUE))
    fileCopy<-data.frame(success = t(fileCopy)[1,])
    fileCopy$path<-paste(pathResults,"/",filesList,sep="")
    fileCopy<-fileCopy[which(fileCopy$success==FALSE),]
    
    if (nrow(fileCopy)!=0){
      for (x in fileCopy$path){
        message(cat("MISSING CONTROL FILES.\n \n",x,"\n \nRUN EXECUTION TERMINATED",sep=""))
      }
      complete<-FALSE
      assign("ErrorOccured","yes",envir = .GlobalEnv)
      assign("ErrorOccured","yes",envir = parent.frame())
    }else{
    complete<-TRUE}

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("createDirs.R",batch_mode)
      }
    }else{#if no error
      return(complete)
      options(warn=0)
    }#end if error
    
  }#test if previous error
}#end function
