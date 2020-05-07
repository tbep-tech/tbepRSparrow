#'@title createInitialParameterControls
#'@description Function to creates parameters.csv file and design_matrix.csv based on the varType column of the dataDictionary.csv file,
#'Opens the new parameters.csv  and design_matrix.csv files for the user to edit.
#'Terminates execution of RSPARROW.
#'Uses subroutines: errorOccurred.
#'@param file_sum user specified run_id 
#'@param path_results path to results directory
#'@param csv_decimalSeparator decimal separator for csv output
#'@param csv_columnSeparator column separator for csv output
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`



createInitialParameterControls<-function(file_sum,path_results,csv_decimalSeparator, csv_columnSeparator,
                                         batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({

      
      exit <- function() {
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }
      
  #load varnames
#test if file exists if not design and betas cannot be created
if (file.exists(file.path(paste(dirname(path_results),"/dataDictionary.csv",sep="")))==TRUE){
  filein <- file.path(paste(dirname(path_results),"/dataDictionary.csv",sep=""))
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
    exit()
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
  
  
   varnames<-read.csv(file=filein,header=TRUE,colClasses=Ctype,
                       dec = csv_decimalSeparator,sep=csv_columnSeparator)
   
   varnames<-varnames[1:(length(Ctype)-numberFields)]
   
    #remove columns/rows with all missing
    varnames<-varnames[apply(varnames,1, function(x) any(!is.na(x))),]
   # varnames<-varnames[,apply(varnames,2, function(x) any(!is.na(x)))]
    
    names(varnames)<-NAMES
    
    varnames<-varnames[which(!is.na(varnames$sparrowNames) & varnames$sparrowNames!=""),]  
    
    #get relavant types
    allTypes<-c("SOURCE","DELIVF","STRM","RESV")
    betaTypes<-varnames[which(varnames$varType %in% allTypes),]
    missingTypes<-betaTypes[which(!betaTypes$varType %in% allTypes),] 
    
    #test if all missing types
    if(nrow(betaTypes)!=0){
    
    #test if previous design matrix exists
    if (file.exists(file.path(paste(dirname(path_results),"/design_matrix.csv",sep="")))==FALSE){
    
    #create initial design Matrix
    initialdesignMatrix<-data.frame(sparrowNames=betaTypes[which(betaTypes$varType=="SOURCE"),]$sparrowNames)
    delivery<-betaTypes[which(betaTypes$varType=="DELIVF"),]$sparrowNames
    initialdesignMatrix<-cbind(initialdesignMatrix,
                               as.data.frame(matrix(rep(0,nrow(initialdesignMatrix)),
                                                    ncol=length(delivery),nrow=nrow(initialdesignMatrix))))
    names(initialdesignMatrix)<-c("sparrowNames",as.character(delivery))
    
    #write file
    fwrite(file=paste(dirname(path_results),"/design_matrix.csv",sep=""),initialdesignMatrix,
           row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
    cat("\n \n")
    message(paste("INITIAL DESIGN_MATRIX FILE : ",paste(dirname(path_results),"/design_matrix.csv",sep="")," AVAILABLE FOR EDIT",sep=""))
    shell.exec(paste(dirname(path_results),"/design_matrix.csv",sep=""))
    #report missing SOURCE or DELIVF types
      missing<-as.character(missingTypes[which(missingTypes$varType %in% c("SOURCE","DELIVF")),]$varType)
    cat("\n \n")
    if (length(missing)!=0){
      message("MISSING varTypes FOR CREATING INITIAL DESIGN MATRIX :")
      for (i in missing){
        message(i)
      }
    }#if missing
    
    #if betas exists test for mismatches in SOURCE and DELIVF
    if (file.exists(file.path(paste(dirname(path_results),"parameters.csv",sep="")))==TRUE){
      filebetas <- file.path(paste(dirname(path_results),"parameters.csv",sep=""))
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
        exit()
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
      
      
      betas<-read.csv(file=filebetas,header=TRUE,colClasses=Ctype,
                     dec = csv_decimalSeparator,sep=csv_columnSeparator)
      
      betas<-betas[1:(length(Ctype)-numberFields)]
      
     #remove columns/rows with all missing
     betas<-betas[apply(betas,1, function(x) any(!is.na(x))),]
    # betas<-betas[,apply(betas,2, function(x) any(!is.na(x)))]
     
     names(betas)<-NAMES
     
     sources<-as.character(betas[which(betas$parmType=="SOURCE"),]$sparrowNames)
     delivery<-as.character(betas[which(betas$parmType=="DELIVF"),]$sparrowNames)
     missingSources<-sources[which(!sources %in% initialdesignMatrix$sparrowNames)]
     missingDelivery<-delivery[which(!delivery %in% names(initialdesignMatrix))]
     #report missing SOURCE or DELIVF variables
     cat("\n \n")
     if (length(missingSources)!=0){
       message("MISSING SOURCE VARIABLES FOUND IN PARAMETERS CONTROL FILE :")
       for (i in missingSources){
         message(i)
       }
     }#if missingSources
     if (length(missingDelivery)!=0){
       cat("\n \n")
       message("MISSING DELIVF VARIABLES FOUND IN PARAMETERS CONTROL FILE :")
       for (i in missingDelivery){
         message(i)
       }
     }#if missingDelivery

  }else{ #if betas file does not exist create it
#create get variables and types
for (t in allTypes[which(allTypes %in% betaTypes$varType)]){
  if (t == allTypes[which(allTypes %in% betaTypes$varType)][1]){
    initialBetas<-data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                             description = betaTypes[which(betaTypes$varType==t),]$explanation,
                             parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                             parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames)))
  }else{
    initialBetas<-rbind(initialBetas,
                        data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                                   description = betaTypes[which(betaTypes$varType==t),]$explanation,
                                   parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                                   parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames))))
  }
}
#add other columns
initialBetas<-cbind(initialBetas,as.data.frame(matrix(rep(0,nrow(initialBetas)),ncol=5,nrow=nrow(initialBetas))))
names(initialBetas)[5:length(initialBetas)]<-c("parmInit","parmMin","parmMax","parmScale","parmCorrGroup")  
#order columns
initialBetas<-initialBetas[,match(c("sparrowNames","description","parmUnits","parmInit","parmMin",
                                    "parmMax","parmType","parmScale","parmCorrGroup"),names(initialBetas))]

#write file
fwrite(file=paste(dirname(path_results),"/parameters.csv",sep=""),initialBetas,
       row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
cat("\n \n")
message(paste("INITIAL PARAMETERS FILE : ",paste(dirname(path_results),"/parameters.csv",sep="")," AVAILABLE FOR EDIT",sep=""))
shell.exec(paste(dirname(path_results),"/parameters.csv",sep=""))

#report for missing types STRM or RESV
missing<-as.character(missingTypes[which(missingTypes$varType %in% c("STRM","RESV")),]$varType)
cat("\n \n")
if (length(missing)!=0){
  message("MISSING varTypes FOR CREATING INITIAL PARAMETERS FILE :")
  for (i in missing){
    message(i)
  }
}#if missing
cat("\n \n")
message("RUN EXECUTION TERMINATED")
exit()
}#if no betas or design
    
}else{#if no design
  cat("\n \n")
  message(paste(paste(dirname(path_results),"/design_matrix.csv",sep="")," ALREADY EXISTS.\n
NEW DESIGN_MATRIX FILE NOT CREATED.\n
SET create_initial_parameterControlFiles<-'no' to RUN RSPARROW WITH CURRENT DESIGN_MATRIX.",sep=""))
  if (exists("initialBetas")==TRUE){
  cat("\n \n")
  message("RUN EXECUTION TERMINATED")
  exit()
  }else{#if betas exist terminate
    cat("\n \n")
    }
}#end if no design

#####################################################################################################################
if (exists("initialBetas")==FALSE){    
#test if previous betas exists
if (file.exists(file.path(paste(dirname(path_results),"/parameters.csv",sep="")))==FALSE){
  #create get variables and types
  for (t in allTypes[which(allTypes %in% betaTypes$varType)]){
    if (t == allTypes[which(allTypes %in% betaTypes$varType)][1]){
      initialBetas<-data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                               description = betaTypes[which(betaTypes$varType==t),]$explanation,
                               parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                               parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames)))
    }else{
      initialBetas<-rbind(initialBetas,
                          data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                                     description = betaTypes[which(betaTypes$varType==t),]$explanation,
                                     parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                                     parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames))))
    }
  }
  #add other columns
  initialBetas<-cbind(initialBetas,as.data.frame(matrix(rep(0,nrow(initialBetas)),ncol=5,nrow=nrow(initialBetas))))
  names(initialBetas)[5:length(initialBetas)]<-c("parmInit","parmMin","parmMax","parmScale","parmCorrGroup")  
  #order columns
  initialBetas<-initialBetas[,match(c("sparrowNames","description","parmUnits","parmInit","parmMin",
                                      "parmMax","parmType","parmScale","parmCorrGroup"),names(initialBetas))]
  
  #write file
  fwrite(file=paste(dirname(path_results),"/parameters.csv",sep=""),initialBetas,
         row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
  cat("\n \n")
  message(paste("INITIAL PARAMETERS FILE : ",paste(dirname(path_results),"/parameters.csv",sep="")," AVAILABLE FOR EDIT",sep=""))
  shell.exec(paste(dirname(path_results),"/parameters.csv",sep=""))
  
  #report for missing types STRM or RESV
  missing<-as.character(missingTypes[which(missingTypes$varType %in% allTypes),]$varType)
  cat("\n \n")
  if (length(missing)!=0){
    message("MISSING varTypes FOR CREATING INITIAL PARAMETERS FILE :")
    for (i in missing){
      message(i)
    }
  }#if missing
 
#test for missing source or delivf variables found in design matrix
  if (exists("initialdesignMatrix")==FALSE){
    initialdesignMatrix<-read.csv(file=file.path(paste(dirname(path_results),"design_matrix.csv",sep="")),
                                  dec = csv_decimalSeparator,sep=csv_columnSeparator)

    #remove columns/rows with all missing
    initialdesignMatrix<-as.data.frame(matrix(initialdesignMatrix[apply(initialdesignMatrix,1, function(x) any(!is.na(x))),],
                                              ncol=ncol(initialdesignMatrix),nrow=nrow(initialdesignMatrix),
                                              dimnames = list(rownames(initialdesignMatrix),colnames(initialdesignMatrix))))
    initialdesignMatrix<-as.data.frame(matrix(initialdesignMatrix[apply(initialdesignMatrix,2, function(x) any(!is.na(x))),],
                                              ncol=ncol(initialdesignMatrix),nrow=nrow(initialdesignMatrix),
                                              dimnames = list(rownames(initialdesignMatrix),colnames(initialdesignMatrix))))
    #initialdesignMatrix<-initialdesignMatrix[apply(initialdesignMatrix,1, function(x) any(!is.na(x))),]
    #initialdesignMatrix<-initialdesignMatrix[,apply(initialdesignMatrix,2, function(x) any(!is.na(x)))]
    
  }
  sources<-as.character(initialBetas[which(initialBetas$parmType=="SOURCE"),]$sparrowNames)
  delivery<-as.character(initialBetas[which(initialBetas$parmType=="DELIVF"),]$sparrowNames)
    missingSources<-initialdesignMatrix$sparrowNames[which(!initialdesignMatrix$sparrowNames %in% sources)]
    missingDelivery<-names(initialdesignMatrix)[which(!names(initialdesignMatrix) %in% c("sparrowNames",delivery))]
    #report missing SOURCE or DELIVF variables
    cat("\n \n")
    if (length(missingSources)!=0){
      message("MISSING SOURCE VARIABLES FOUND IN DESIGN_MATRIX CONTROL FILE :")
      for (i in missingSources){
        message(i)
      }
    }#if missingSources
    if (length(missingDelivery)!=0){
      cat("\n \n")
      message("MISSING DELIVF VARIABLES FOUND IN DESIGN_MATRIX CONTROL FILE :")
      for (i in missingDelivery){
        message(i)
      }
    }#if missingDelivery

  cat("\n \n")
  message("RUN EXECUTION TERMINATED")
  exit()
}else{#if no betas file
  cat("\n \n")
message(paste(paste(dirname(path_results),"/parameters.csv",sep="")," ALREADY EXISTS.\n
NEW PARAMETERS FILE NOT CREATED.\n
SET create_initial_parameterControlFiles<-'no' to RUN RSPARROW WITH CURRENT PARAMETERS FILE.\n
RUN EXECUTION TERMINATED.",sep=""))
exit()
}#end if not betas file
}#if no initialBetas

}else{# if all missing types
    message("NO VALID varTypes FOUND.\n
NEW PARAMETER CONTROL FILES NOT CREATED.\n
SET varType = 'SOURCE','DELIVF','STRM', or, 'RESV' TO CREATE PARAMETER CONTROL FILES. \n
RUN EXECUTION TERMINATED.")
  exit()
}
}else{#no varnames file
    message("NO dataDictionary FILE FOUND.\n
NEW PARAMETER CONTROL FILES NOT CREATED.\n
SET create_initial_parameterControlFiles<-'no' to RUN RSPARROW WITH CURRENT PARAMETER CONTROL FILES.\n
RUN EXECUTION TERMINATED.")
  exit()
  }#end no varnames      

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("createInitialParameterControls.R",batch_mode)
      }
    }else{#if no error

    }#end if error
    
  }#test if previous error
}#end function