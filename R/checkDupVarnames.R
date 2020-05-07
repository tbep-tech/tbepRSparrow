#'@title checkDupVarnames
#'@description Checks for duplicate sparrowNames in the dataDictionary.csv file 
#'and outputs list of duplicates as message in console
#'Uses subroutines: errorOccurred. 
#'@param data_names input data from data_Dictionary.csv file
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`

checkDupVarnames<-function(data_names,batch_mode,ErrorOccured){
  #test for duplicate sparrowNames
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  dupSparrow<-data_names$sparrowNames[duplicated(data_names$sparrowNames)]
  msgDupSparrow<-""
  if (length(dupSparrow)!=0){
    for (i in dupSparrow){
    message(paste("Duplicate sparrowNames found : ",i,sep="\n"))
    if (batch_mode=="yes"){
      cat(paste("Duplicate sparrowNames found : ",i,sep="\n"))
      if (i==dupSparrow[length(dupSparrow)]){
        cat("\n \n")
      }
    }      
    }

  }
 #test for duplicate data1UserNames 
  msgDupUserNames<-""
  dupUserNames<-data_names$data1UserNames[duplicated(data_names$data1UserNames)]
  dupUserNames<-dupUserNames[which(!is.na(dupUserNames))]
  if (length(dupUserNames)!=0){
    for (i in dupUserNames){
    message(paste("Duplicate data1UserNames found : ",i,sep="\n"))
    if (batch_mode=="yes"){
      cat(paste("Duplicate data1UserNames found : ",i,sep="\n"))
      if (i==dupUserNames[length(dupUserNames)]){
        cat("\n \n")
      }
    }      
    }

  }
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("checkDupVarnames.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function