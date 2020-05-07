#
# readParameters.R
#
###########################################################
# Read parameter CSV file
###########################################################

readParameters <- function(path,filename,csv_decimalSeparator, csv_columnSeparator,if_estimate,
                           if_estimate_simulation,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 
      exit <- function() {
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }
      

  filebetas <- paste(path,filename,"_parameters.csv",sep="")
  Ctype <- c("character","character","character","numeric","numeric","numeric","character","numeric","numeric")
  NAMES<- c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmScale","parmCorrGroup")   
  
  
  numberFields<-max(count.fields(filebetas,sep=csv_columnSeparator))
  numberFields<-numberFields-length(Ctype)
  if (numberFields>0){
    Ctype<-c(Ctype,rep("NULL",numberFields))
  }else if (numberFields<0){#not enough columns invalid file
    message(paste("ERROR: INVALID PARAMETERS FILE CHECK NUMBER OF COLUMNS\n 
                  PARAMETERS FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n ",sep=""))
    for (i in NAMES){
      message(i)
    }
    
    message("\n \nRUN EXECUTION TERMINATED")
    
    if (batch_mode=="yes"){
      cat("ERROR: INVALID PARAMETERS FILE CHECK NUMBER OF COLUMNS\n 
          PARAMETERS FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n ",sep="")
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
  
  betavalues <- read.csv(filebetas,header=TRUE,colClasses=Ctype,
                         dec = csv_decimalSeparator,sep=csv_columnSeparator)
  betavalues<-betavalues[1:(length(Ctype)-numberFields)]
  
  #remove columns/rows with all missing
  betavalues<-betavalues[apply(betavalues,1, function(x) any(!is.na(x))),]
  #betavalues<-betavalues[,apply(betavalues,2, function(x) any(!is.na(x)))]
  
  names(betavalues)<-NAMES
  
  
  #trim whitespaces
  betavalues$sparrowNames<-trimws(betavalues$sparrowNames,which="both")
  #make fixed and required names lowercase
  betavalues$sparrowNames<-ifelse(tolower(betavalues$sparrowNames) %in% as.character(getVarList()$varList),tolower(betavalues$sparrowNames),betavalues$sparrowNames)
  
  #replace NAs with 0 in numeric columns
  for (c in names(betavalues)){
    test<-eval(parse(text=paste("betavalues$",c,sep="")))
    if (class(test)=="numeric"){
      test<-ifelse(is.na(test),0,test)
      eval(parse(text=paste("betavalues$",c,"<-test",sep="")))
    }
  }
  
  #create parmConstant
  betavalues$parmConstant<-ifelse(betavalues$parmInit==betavalues$parmMax & betavalues$parmInit==betavalues$parmMin & betavalues$parmInit!=0,1,0)
  betavalues<-betavalues[,match(c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmScale","parmConstant","parmCorrGroup"),names(betavalues))]
  
  #test for "SOURCE" in parmType column
  sources<-betavalues[which(betavalues$parmType=="SOURCE"),]
  if (nrow(sources)==0){
    ErrorOccured<-"yes"
    message("NO SOURCES FOUND IN PARAMETERS FILE.\nRUN EXECUTION TERMINATED.")
    if (batch_mode=="yes"){#if batch output message to log
      cat("NO SOURCES FOUND IN PARAMETERS FILE.\nRUN EXECUTION TERMINATED.")
    }
    assign("ErrorOccured","yes",envir = .GlobalEnv)
    assign("ErrorOccured","yes",envir = parent.frame())
    exit()
  }
  
  #test for no parameters (parmMax==0)
  testMax<-betavalues[which(betavalues$parmMax!=0),]
  if (nrow(testMax)==0 & (if_estimate=="yes" |if_estimate_simulation=="yes")){
    ErrorOccured<-"yes"
    message("NO PARAMETERS FOUND FOR ESTIMATION IN PARAMETERS FILE.\nALL PARAMETERS FOUND HAVE parmMAX==0\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
    if (batch_mode=="yes"){#if batch output message to log
      cat("NO PARAMETERS FOUND FOR ESTIMATION IN PARAMETERS FILE.\nALL PARAMETERS FOUND HAVE parmMAX==0\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
    }
    assign("ErrorOccured","yes",envir = .GlobalEnv)
    assign("ErrorOccured","yes",envir = parent.frame())
    exit()
  }
  
  #test for missing values
  missing<-character(0)
  for (c in 4:length(betavalues)){
    testNA<-betavalues[c]
    testNA<-which(is.na(testNA))
    if (length(testNA)!=0){
      missing<-c(missing,names(betavalues)[c])
    }
  }
  if (length(missing)!=0){
    ErrorOccured<-"yes"
    message(paste(" \nMISSING VALUES FOUND IN THE FOLLOWING COLUMNS OF THE PARAMETERS FILE:\n \n",paste(missing,collapse="\n"),"\n \nRUN EXECUTION TERMINATED.",sep=""))
    if (batch_mode=="yes"){#if batch output message to log
      cat(" \nMISSING VALUES FOUND IN THE FOLLOWING COLUMNS OF THE PARAMETERS FILE:\n \n",paste(missing,collapse="\n"),"\n \nRUN EXECUTION TERMINATED.",sep="")
    }
      assign("ErrorOccured","yes",envir = .GlobalEnv)
    assign("ErrorOccured","yes",envir = parent.frame())

    exit() 
  }


  
      },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("readParameters.R",batch_mode)
      }
    }else{#if no error
return(betavalues)
    }#end if error
    
  }#test if previous error
}#end function


