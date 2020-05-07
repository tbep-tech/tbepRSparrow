#'@title checkBinaryMaps
#'@description Checks if binary mapping objects exist and loads binary files if exists
#'Uses subroutines: errorOccurred. 
#'@param mapSetting as.character setting in control file that user sets 
#'@param settingValue setting in control file that user sets 
#'@param outObj object loaded from binary file
#'@param path_gis path to users gis data
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return logical TRUE/FALSE indicating whether or not file is loaded

checkBinaryMaps<-function(mapSetting, settingValue, outObj, path_gis,batch_mode,ErrorOccured){
 
  if (ErrorOccured=="no"){
    tryIt<-try({

   objfile <- paste(path_gis,"/",outObj,sep="")
  
  Setting<-settingValue
  
  if(!is.na(Setting) & file.exists(objfile)) { 
    load(objfile)
    assign(outObj,get(outObj),env = parent.frame())
    
    if (!exists("outObj")){
      message(paste(mapSetting," <- ",Setting," NOT FOUND MAPPING CANNOT COMPLETE.\nSet if_create_binary_maps<-'yes' to create binary files.",sep=""))
      if (batch_mode=="yes"){#if batch output message to log
        cat(mapSetting," <- ",Setting," NOT FOUND MAPPING CANNOT COMPLETE.\nSet if_create_binary_maps<-'yes' to create binary files.",sep="")
      }
      fileLoaded<-FALSE
    }else{
      fileLoaded<-TRUE
    }
  }else{
    fileLoaded<-FALSE
  }
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("checkBinaryMaps.R",batch_mode)
      }
    }else{#if no error
      return(fileLoaded)
    }#end if error
    
  }#test if previous error
}#end function