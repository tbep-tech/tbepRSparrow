#'@title checkAnyMissingSubdataVars
#'@description function to identify required variables and parameter variables with any missing or zero values and print a warning in the console
#'Uses subroutines: getVarList, errorOccurred. 
#'@param subdata input data (subdata) 
#'@param betavalues list of parameters from parameters.csv
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`



checkAnyMissingSubdataVars <- function(subdata,betavalues,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 
#  input variables:
#  subdata
#  betavalues
  datalstCheck <- as.character(getVarList()$varList)

  datalstMissingSubdata <- rep(0,length(datalstCheck))
  k<-0
  for (i in 1:length(datalstCheck)) { 
    dname <- paste("if(sum(is.na(subdata$",datalstCheck[i],"))>0)
                   { k<-k+1; datalstMissingSubdata[k] <- datalstCheck[i] }",sep="")   
    eval(parse(text=dname))  # tag as some values missing
  }
  
  # Check user-selected parameters in SUBDATA for any NAs
  xlnames <- betavalues$sparrowNames[betavalues$parmMax != 0]
  for (i in 1:length(xlnames)) { 
    dname <- paste("if(sum(is.na(subdata$",xlnames[i],"))>0)
                   { k<-k+1; datalstMissingSubdata[k] <- xlnames[i] }",sep="")   
    eval(parse(text=dname))  # tag as some values missing
  }

  if(k>0) { 
    reqMissingSubdataVariable<-datalstMissingSubdata[1:k]
    reqMissingSubdataVariable<-reqMissingSubdataVariable[which(reqMissingSubdataVariable %in% as.character(getVarList()$reqNames))]
    if (length(reqMissingSubdataVariable)!=0){
      for (i in reqMissingSubdataVariable){
      message(paste(" \nWARNING: THIS REQUIRED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n ",sep=""))
        if (batch_mode=="yes"){
          cat(paste(" \nWARNING: THIS REQUIRED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n ",sep=""))
          if (i==reqMissingSubdataVariable[length(reqMissingSubdataVariable)]){
            cat("\n \n")
          }
        }
      }
    }else
      reqMissingSubdataVariableMessage<-""
    
    fixMissingSubdataVariable<-datalstMissingSubdata[1:k]
    fixMissingSubdataVariable<-fixMissingSubdataVariable[which(fixMissingSubdataVariable %in% as.character(getVarList()$fixNames))]
    if (length(fixMissingSubdataVariable)!=0){
      for (i in fixMissingSubdataVariable){
      message(paste(" \nWARNING: THIS FIXED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n ",sep=""))
        if (batch_mode=="yes"){
          cat(paste(" \nWARNING: THIS FIXED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n ",sep=""))
          if (i==fixMissingSubdataVariable[length(fixMissingSubdataVariable)]){
            cat("\n \n")
          }
        }
      }
    }else
      fixMissingSubdataVariableMessage<-""
  
    paramMissingSubdataVariable<-datalstMissingSubdata[1:k]
    paramMissingSubdataVariable<-paramMissingSubdataVariable[which(paramMissingSubdataVariable %in% as.character(xlnames))]
    if (length(paramMissingSubdataVariable)!=0){
      for (i in paramMissingSubdataVariable){
        message(paste(" \nERROR: THIS PARAMETER VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\nRUN EXECUTION TERMINATED",sep=""))
        if (batch_mode=="yes"){
          cat(paste(" \nERROR: THIS PARAMETER VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\nRUN EXECUTION TERMINATED",sep=""))
          if (i==paramMissingSubdataVariable[length(paramMissingSubdataVariable)]){
            cat("\n \n")
          }
        }
        assign("ErrorOccured","yes",envir = .GlobalEnv)
        assign("ErrorOccured","yes",envir = parent.frame())
      }
    }else
      paramMissingSubdataMessage<-""
    
  }
  
  if (ErrorOccured=="yes"){
    exit <- function() {
      .Internal(.invokeRestart(list(NULL, NULL), NULL))
    }
    exit()  
  }

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("checkAnyMissingSubdataVars.R",batch_mode)
      }
    }else{#if no error
    }#end if error
    
  }#test if previous error
}#end function




