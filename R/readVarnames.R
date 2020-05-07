#
# readVarnames.R
#
###########################################################
# IDENTIFY VARIABLE NAMES IN DATA1 FILE
###########################################################
# Transfer the required and additional variables to a standard object name
# make all fixed/required sparrowNames lowercase
# trim whitespace from beginning and end of sparrowNames
# check for and remove rows with blank sparrowNames
# remove all duplicated rows

readVarnames <- function(path,filename,csv_decimalSeparator, csv_columnSeparator,
                         batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 
      exit <- function() {
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }
      

  filein <- paste(path,filename,"_dataDictionary.csv",sep="")
  Ctype <- c("character","character","character","character","character")
  NAMES<-c("varType","sparrowNames","data1UserNames","varunits","explanation")
  
  numberFields<-max(count.fields(filein,sep=csv_columnSeparator))
  numberFields<-numberFields-length(Ctype)
  if (numberFields>0){
    Ctype<-c(Ctype,rep("NULL",numberFields))
  }else if (numberFields<0){#not enough columns invalid file
    message(paste("ERROR: INVALID dataDictionary FILE CHECK NUMBER OF COLUMNS\n 
                  dataDictionary FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n ",sep=""))
    for (i in NAMES){
      message(i)
    }
    
    message("\n \nRUN EXECUTION TERMINATED")
    
    if (batch_mode=="yes"){
      cat("ERROR: INVALID dataDictionary FILE CHECK NUMBER OF COLUMNS\n 
          dataDictionary FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n ",sep="")
      for (i in NAMES){
        cat(i)
      }
      
      cat("\n \nRUN EXECUTION TERMINATED")
    }
    ErrorOccured<-"yes"
    assign("ErrorOccured","yes",envir = .GlobalEnv)
    assign("ErrorOccured","yes",envir = parent.frame())
    exit()
    }
  
  
  data_names <- read.csv(filein,header=TRUE,colClasses=Ctype,
                         dec = csv_decimalSeparator,sep=csv_columnSeparator)
  
  data_names<-data_names[1:(length(Ctype)-numberFields)]
  
  #remove columns/rows with all missing
  data_names<-data_names[apply(data_names,1, function(x) any(!is.na(x))),]
 # data_names<-data_names[,apply(data_names,2, function(x) any(!is.na(x)))]
  
  names(data_names)<-NAMES
  
  #trim whitespaces
  data_names$sparrowNames<-trimws(data_names$sparrowNames,which="both")
  data_names$data1UserNames<-trimws(data_names$data1UserNames,which="both")  
  #make fixed and required names lowercase
  data_names$sparrowNames<-ifelse(tolower(data_names$sparrowNames) %in% as.character(getVarList()$varList),tolower(data_names$sparrowNames),data_names$sparrowNames)

  
  blankSparrow<-data_names[which(is.na(data_names$sparrowNames)|data_names$sparrowNames==""),]
  if (nrow(blankSparrow)!=0){
    message(" \nsparrowName is BLANK in data dictionary at row(s) : ", paste(rownames(blankSparrow),collapse=", "),".  These rows have been removed.",sep="")
    cat("\n \n")
    if (batch_mode=="yes"){
      cat(" \nsparrowName is BLANK in data dictionary at row(s) : ", paste(rownames(blankSparrow),collapse=", "),".  These rows have been removed.",sep="")
      cat("\n \n")     
    }
  }
    
  data_names<-data_names[which(!is.na(data_names$sparrowNames) & data_names$sparrowNames!=""),]
    #remove exact duplicates
  data_names<-data_names[!duplicated(data_names),]
 
  #test if add_vars in data_names
  if(!is.na(add_vars)){
  if (any(!add_vars %in% data_names$sparrowNames)){
    message(paste0("WARNING: add_vars MISSING FROM dataDictionary sparrowNames : ",paste(add_vars[which(!add_vars %in% data_names$sparrowNames)],collapse=","),"\n \n"))
    cat("\n \n")   
    if (batch_mode=="yes"){
      cat("WARNING: add_vars MISSING FROM dataDictionary sparrowNames : ",paste(add_vars[which(!add_vars %in% data_names$sparrowNames)],collapse=","),"\n \n",sep="")
      cat("\n \n")     
    }
    add_vars<-add_vars[which(add_vars %in% data_names$sparrowNames)]
  }
  }
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("readVarnames.R",batch_mode)
      }
    }else{#if no error
 return(data_names)
    }#end if error
    
  }#test if previous error
}#end function
