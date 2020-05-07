#'@title checkMissingData1Vars
#'@description Identify required variables with all missing/zero values in data1 and missing required variables
#'Adds missing variables to DATA1 with all NA values warning to user.
#'Outputs list of missing variables as message in console
#'Uses subroutines: errorOccurred. 
#'@param data1 input data (data1) 
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return data1 with missing variables added

checkMissingData1Vars <- function(data1,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  #get fixed/required variable names
  datalstCheck<-as.character(getVarList()$varList)
  
  datalstMissing <- rep(0,length(datalstCheck))
  k<-0
  for (i in 1:length(datalstCheck)) { 
    if((datalstCheck[i] %in% colnames(data1)) == FALSE) {   # check for existence of variable in data1
      dname <- paste("data1$",datalstCheck[i],"<-NA",sep="") 
      eval(parse(text=dname))  # place missing variable in data1 object
      k<-k+1
      datalstMissing[k] <- datalstCheck[i]  # list missing variable
    } else {  # check for all missing values
      dname <- paste("if(all(is.na(data1$",datalstCheck[i],"))==TRUE) { k<-k+1; datalstMissing[k] <- datalstCheck[i] }",sep="")   
      eval(parse(text=dname))  # tag as all values missing
    }
  }
  MissingData1VariableMessage <- ""
  reqMissingData1VariableMessage <- ""
  fixMissingData1VariableMessage <- ""
  
  if(k>0) { 
    reqMissingData1Variable<-datalstMissing[1:k]
    reqMissingData1Variable<-reqMissingData1Variable[which(reqMissingData1Variable %in% as.character(getVarList()$reqNames))]
    if (length(reqMissingData1Variable)!=0){
      for (i in reqMissingData1Variable){
    reqMissingData1VariableMessage <- paste(" \nWARNING: THIS REQUIRED VARIABLE HAS ALL MISSING VALUES IN DATA1:",i,"\n ",sep="")
      }
    }else
      reqMissingData1VariableMessage<-""
    
    fixMissingData1Variable<-datalstMissing[1:k]
    fixMissingData1Variable<-fixMissingData1Variable[which(fixMissingData1Variable %in% as.character(getVarList()$fixNames))]
    if (length(fixMissingData1Variable)!=0){
      for (i in fixMissingData1Variable){
    fixMissingData1VariableMessage <- paste(" \nWARNING: THIS FIXED VARIABLE HAS ALL MISSING VALUES IN DATA1:",i,"\n ",sep="")
      }
    }else
      fixMissingData1VariableMessage<-""
  }

  #output messages
  if (reqMissingData1VariableMessage!=""){
    cat("\n\n")
    message(reqMissingData1VariableMessage)
    cat("\n\n")
    if (batch_mode=="yes"){
      cat(reqMissingData1VariableMessage)
      cat("\n\n")  
    }
  }
  if (fixMissingData1VariableMessage!=""){
    cat("\n\n")
    message(fixMissingData1VariableMessage)
    cat("\n\n")
    if (batch_mode=="yes"){
      cat(fixMissingData1VariableMessage)
      cat("\n\n")  
    }
  }
  
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("checkMissingData1Vars.R",batch_mode)
      }
    }else{#if no error
      return(data1)
    }#end if error
    
  }#test if previous error
}#end function
