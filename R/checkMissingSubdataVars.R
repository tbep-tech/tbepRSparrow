#'@title checkMissingSubdataVars
#'@description Identify required variables and parameter variables with all missing/zero values in subdata
#'Outputs list of variables with all missing values as message in console
#'Uses subroutines: errorOccurred. 
#'@param subdata input data (subdata) 
#'@param betavalues list of parameters from parameters.csv
#'@param path_results path to results directory
#'@param file_sum user specified run_id
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return data1 with missing variables added
#


checkMissingSubdataVars <- function(subdata,betavalues,path_results,file_sum,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 

# input variables:
#  subdata
#  betavalues
  
  datalstCheck <- as.character(getVarList()$varList)
  datalstMissingSubdata <- rep(0,length(datalstCheck))
  k<-0
  for (i in 1:length(datalstCheck)) { 
    dname <- paste("if(all(is.na(subdata$",datalstCheck[i],"))==TRUE | all(subdata$",datalstCheck[i]," == 0))
                   { k<-k+1; datalstMissingSubdata[k] <- datalstCheck[i] }",sep="")   
    eval(parse(text=dname))  # tag as all values missing
  }
  
  # Check user-selected parameters in SUBDATA for all NAs
  xlnames <- betavalues$sparrowNames[betavalues$parmMax != 0]
  for (i in 1:length(xlnames)) { 
    dname <- paste("if(all(is.na(subdata$",xlnames[i],"))==TRUE | 
                   all(subdata$",xlnames[i]," == 0))
                   { k<-k+1; datalstMissingSubdata[k] <- xlnames[i] }",sep="")   
    eval(parse(text=dname))  # tag as all values missing
  }
  
  # Check sparrowNames from varnames in SUBDATA for all NAs
  vrnames <- data_names$sparrowNames
  for (i in 1:length(vrnames)) { 
    dname <- paste("if(all(is.na(subdata$",vrnames[i],"))==TRUE | 
                   all(subdata$",vrnames[i]," == 0))
                   { k<-k+1; datalstMissingSubdata[k] <- vrnames[i] }",sep="")   
    eval(parse(text=dname))  # tag as all values missing
  }

  MissingSubdataVariableMessage <- ""
  if(k>0) {
  
    fixMissingSubdataVariable<-datalstMissingSubdata[1:k]
    for (i in unique(fixMissingSubdataVariable)){
    if(i %in% xlnames & i %in% datalstCheck & i %in% vrnames){
      problemFile<-paste("BOTH the ",path_results,file_sum,"_parameters.csv and 
                         the ",path_results,file_sum,"_dataDictionary.csv files",sep="")
    }else if (i %in% datalstCheck){
      problemFile<-"The required and fixed variables list"
    }else if (i %in% xlnames){
      problemFile<-paste("The ",path_results,file_sum,"_parameters.csv file",sep="")
    }else{
      problemFile<-paste("The ",path_results,file_sum,"_dataDictionary.csv file",sep="")
    }
      if (i %in% c(getVarList()$reqNames,xlnames)){
        msgText<-paste(" \nERROR: THIS REQUIRED VARIABLE FROM \n",problemFile,
                       " \nHAS ALL MISSING OR ZERO VALUES IN SUBDATA:",i,"\n \nRUN EXECUTION TERMINATED.\n ",sep="")
        assign("ErrorOccured","yes",envir = .GlobalEnv)
        assign("ErrorOccured","yes",envir = parent.frame())
      }else{
        msgText<-paste(" \nWARNING: THIS REQUIRED VARIABLE FROM \n",problemFile,
                       " \nHAS ALL MISSING OR ZERO VALUES IN SUBDATA:",i,"\n ",sep="")
      }
    message(msgText)
    
    if (batch_mode=="yes"){
      cat(msgText)
      if (i==fixMissingSubdataVariable[length(fixMissingSubdataVariable)]){
      cat("\n \n")
      }
    }
  }
}
  

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("checkMissingSubdataVars.R",batch_mode)
      }
    }else{#if no error

    }#end if error
    
  }#test if previous error
}#end function
