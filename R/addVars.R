#'@title addVars
#'@description Tests for sparrowNames found in parameters.csv, but not in dataDictionary.csv or design_matrix.csv.
#'edits dataDictionary.csv and/or design_matrix.csv adding missing sparrowName 
#'and opens design_matrix.csv and userModifyData.R for edit.
#'Uses subroutines: errorOccurred. 
#'@param path_user this is path to users model directory containing results, data, and gis folders
#'@param run_id user specified run_id 
#'@param results_directoryName this is the users directory name for the results
#'@param csv_decimalSeparator decimal separator for read/write csv files
#'@param csv_columnSeparator column separator for read/write csv files
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`


addVars<-function(path_user,results_directoryName,csv_decimalSeparator,csv_columnSeparator,
                   batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 
 
  
      exit <- function() {
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }
      
      #read parameters file
      filebetas<-paste(path_user,"/",results_directoryName,"/parameters.csv",sep="")
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
      
 # filebetas<-paste(path_user,"/",results_directoryName,"/parameters.csv",sep="")
#  Ctype <- c("character","character","character","numeric","numeric","numeric","character","numeric","numeric")
#  betavalues <- read.csv(filebetas,header=TRUE,colClasses=Ctype,
#                         dec = csv_decimalSeparator,sep=csv_columnSeparator)
#  #remove columns/rows with all missing
#  betavalues<-betavalues[apply(betavalues,1, function(x) any(!is.na(x))),]
#  betavalues<-betavalues[,apply(betavalues,2, function(x) any(!is.na(x)))]
  
#  names(betavalues)<-c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmScale","parmCorrGroup")
  
  #trim whitespaces
#  betavalues$sparrowNames<-trimws(betavalues$sparrowNames,which="both")  
  #make fixed and required names lowercase
#  betavalues$sparrowNames<-ifelse(tolower(betavalues$sparrowNames) %in% as.character(getVarList()$varList),tolower(betavalues$sparrowNames),betavalues$sparrowNames)
 
       
  #read dataDictionary
      filein <- paste(path_user,"/",results_directoryName,"/dataDictionary.csv",sep="")
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
      
  fileDic<-paste(path_user,"/",results_directoryName,"/dataDictionary.csv",sep="")
  #Ctype <- c("character","character","character","character","character")
  #data_names <- read.csv(fileDic,header=TRUE,colClasses=Ctype,
   #                      dec = csv_decimalSeparator,sep=csv_columnSeparator)
  #remove columns/rows with all missing
  #data_names<-data_names[apply(data_names,1, function(x) any(!is.na(x))),]
  #data_names<-data_names[,apply(data_names,2, function(x) any(!is.na(x)))]
  
  
  #trim whitespaces
#  data_names$sparrowNames<-trimws(data_names$sparrowNames,which="both")
#  data_names$data1UserNames<-trimws(data_names$data1UserNames,which="both")
    
  #make fixed and required names lowercase
 # data_names$sparrowNames<-ifelse(tolower(data_names$sparrowNames) %in% as.character(getVarList()$varList),tolower(data_names$sparrowNames),data_names$sparrowNames)

  
  #read designMatrix
      filed <- paste(path_user,"/",results_directoryName,"/design_matrix.csv",sep="")
      
      #columns for DELIVF
      Ctype<-nrow(betavalues[which(betavalues$parmType=="DELIVF"),])
      NAMES<-betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames
      
      numberFields<-max(count.fields(filed,sep=csv_columnSeparator))
      numberFields<-numberFields-Ctype
      if (numberFields>0){
        Ctype<-c(betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames,rep("NULL",numberFields))
      }else if (numberFields<0){#not enough columns invalid file
        message(paste("ERROR: INVALID DESIGN_MATRIX FILE CHECK NUMBER OF COLUMNS\n 
                      DESIGN_MATRIX FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n ",sep=""))
        for (i in NAMES){
          message(i)
        }
        
        message("\n \nRUN EXECUTION TERMINATED")
        
        if (batch_mode=="yes"){
          cat("ERROR: INVALID DESIGN_MATRIX FILE CHECK NUMBER OF COLUMNS\n 
              DESIGN_MATRIX FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n ",sep="")
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
      
      
      dmatrixin <- read.csv(filed,header=TRUE,row.names=1,dec = csv_decimalSeparator,sep=csv_columnSeparator)
      dmatrixin<-dmatrixin[1:(length(Ctype)-numberFields)]
      
      #remove columns/rows with all missing
      dmatrixin<-as.data.frame(matrix(dmatrixin[apply(dmatrixin,1, function(x) any(!is.na(x))),],ncol=ncol(dmatrixin),nrow=nrow(dmatrixin),dimnames = list(rownames(dmatrixin),colnames(dmatrixin))))
      #dmatrixin<-dmatrixin[,apply(dmatrixin,2, function(x) any(!is.na(x)))]
      
      names(dmatrixin)<-NAMES
      
      #trim whitespaces
      rownames(dmatrixin)<-trimws(rownames(dmatrixin),which="both")
      names(dmatrixin)<-trimws(names(dmatrixin),which="both")
      #make fixed and required names lowercase
      rownames(dmatrixin)<-ifelse(tolower(rownames(dmatrixin)) %in% as.character(getVarList()$varList),tolower(rownames(dmatrixin)),rownames(dmatrixin))
      names(dmatrixin)<-ifelse(tolower(names(dmatrixin)) %in% as.character(getVarList()$varList),tolower(names(dmatrixin)),names(dmatrixin))
      

  fileDesign<-paste(path_user,"/",results_directoryName,"/design_matrix.csv",sep="")
#  dmatrixin <- read.csv(fileDesign,header=TRUE,row.names=1,dec = csv_decimalSeparator,sep=csv_columnSeparator)
#  #remove columns/rows with all missing
#  dmatrixin<-dmatrixin[apply(dmatrixin,1, function(x) any(!is.na(x))),]
#  dmatrixin<-dmatrixin[,apply(dmatrixin,2, function(x) any(!is.na(x)))]
  
  
  #trim whitespaces
 # rownames(dmatrixin)<-trimws(rownames(dmatrixin),which="both")
#  names(dmatrixin)<-trimws(names(dmatrixin),which="both")
  #make fixed and required names lowercase
 # rownames(dmatrixin)<-ifelse(tolower(rownames(dmatrixin)) %in% as.character(getVarList()$varList),tolower(rownames(dmatrixin)),rownames(dmatrixin))
#  names(dmatrixin)<-ifelse(tolower(names(dmatrixin)) %in% as.character(getVarList()$varList),tolower(names(dmatrixin)),names(dmatrixin))
  
  
  #test for parameters NOT in dataDictionary
  test_data_names<-as.data.frame(betavalues[which(!betavalues$sparrowNames %in% data_names$sparrowNames),])
  names(test_data_names)<-names(betavalues)
  if (nrow(test_data_names)!=0){#missing from dataDictionary
    for (t in test_data_names$sparrowNames){
      message(paste("ERROR: ",t," PARAMETER NOT FOUND IN dataDictonary.csv\n ",t, 
                    " HAS BEEN ADDED TO dataDictionary.csv\n USER MUST EDIT 
dataDictionary.csv, userModifyData.R, and design_matrix.R\n 
TO ALLOW FOR NEW PARAMETER\n \nDATA IMPORT MUST BE RE_RUN\n
SET run_dataImport<-'yes' AND load_previousDataImport<-'no'\n \n",sep=""))
      new_data_names<-data.frame(varType = test_data_names[which(test_data_names$sparrowNames==t),]$parmType,
                                 sparrowNames=t,
                                 data1UserNames = NA,
                                 varunits = NA,
                                 explanation = test_data_names[which(test_data_names$sparrowNames==t),]$description)
      
      #check against fixed and required list
      if (t %in% getVarList()$reqNames){
        new_data_names$varType<-"REQUIRED"
        explanations<-getVarList()$explanations
        new_data_names$explanation<-as.character(explanations[which(explanations$sparrowNames==t),]$explanation)
      }else if (t %in% getVarList()$fixNames){
        new_data_names$varType<-"FIXED"
        explanations<-getVarList()$explanations
        new_data_names$explanation<-as.character(explanations[which(explanations$sparrowNames==t),]$explanation)
      }
      
      data_names<-rbind(data_names,new_data_names)
    }#end for
    
    #save dataDictionary
    fwrite(data_names,file=fileDic,row.names=F,append=F,quote=T,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
    }#end if test_data_names
  
      #check against design_matrix if SOURCE or DELIVF 
        test_design<-c(rownames(dmatrixin),names(dmatrixin)[which(names(dmatrixin)!="sparrowNames")])
        test_design<-as.data.frame(betavalues[which(!betavalues$sparrowNames %in% test_design
                                                    & betavalues$parmType %in% c("SOURCE","DELIVF")),])
        names(test_design)<-names(betavalues)
        if (nrow(test_design)!=0){#missing from design_matrix
          
          for (t in test_design$sparrowNames){
          if (test_design[which(test_design$sparrowNames==t),]$parmType=="SOURCE"){
            message(paste("ERROR: ",t," PARAMETER NOT FOUND IN design_matrix.csv as SOURCE\n ",t, 
                          " HAS BEEN ADDED TO design_matrix.csv\n USER MUST EDIT design_matrix.csv, userModifyData.R\n 
TO ALLOW FOR NEW PARAMETER\n \nDATA IMPORT MUST BE RE_RUN\n
SET run_dataImport<-'yes' AND load_previousDataImport<-'no'\n \n",sep=""))
            
            new_design_matrix<-as.data.frame(matrix(rep(0,length(dmatrixin)),ncol=length(dmatrixin),nrow=1))
            names(new_design_matrix)<-names(dmatrixin)
            rownames(new_design_matrix)<-t
            
            dmatrixin<-rbind(dmatrixin,new_design_matrix)
          }else{
            message(paste("ERROR: ",t," PARAMETER NOT FOUND IN design_matrix.csv as DELIVF\n ",t, 
                          " HAS BEEN ADDED TO design_matrix.csv\n USER MUST EDIT design_matrix.csv, userModifyData.R\n 
TO ALLOW FOR NEW PARAMETER\n \nDATA IMPORT MUST BE RE_RUN\n
SET run_dataImport<-'yes' AND load_previousDataImport<-'no'\n \n",sep=""))
            
            new_design_matrix<-as.data.frame(matrix(rep(0,nrow(dmatrixin)),ncol=1,nrow=nrow(dmatrixin)))
            names(new_design_matrix)<-t
            
            dmatrixin<-cbind(dmatrixin,new_design_matrix)
          }#end ifelse
          }#end for 
          
          #save design_matrix
          dmatrixin<-cbind(rownames(dmatrixin),dmatrixin)
          names(dmatrixin)[1]<-"sparrowNames"
          
          #order according to parameters
          dmatrixin<-dmatrixin[match(betavalues[which(betavalues$parmType=="SOURCE"),]$sparrowNames,dmatrixin$sparrowNames),]
          dmatrixin<-dmatrixin[,match(c("sparrowNames",betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames),names(dmatrixin))]
          
          fwrite(dmatrixin,file=fileDesign,row.names=F,append=F,quote=F,showProgress = FALSE,
                 dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
          }#end if test_design

        #if missing parameter variables found open files for edit and terminate run
        if (nrow(test_design)!=0 | nrow(test_data_names)!=0){
          message(paste("USER MUST EDIT CONTROL FILES WITH MISSING PARAMETER INFORMATION\ndesign_matrix.csv, dataDictionary.csv, and userModifyData.R ARE OPEN FOR EDIT\n RUN EXECUTION TERMINATED",sep=""))
          shell.exec(paste(path_user,"/",results_directoryName,"/design_matrix.csv",sep=""))
          shell.exec(paste(path_user,"/",results_directoryName,"/dataDictionary.csv",sep=""))
          file.edit(paste(path_user,"/",results_directoryName,"/userModifyData.R",sep=""))
          ErrorOccured<-"yes"
          assign("ErrorOccured","yes",envir = .GlobalEnv)
          assign("ErrorOccured","yes",envir = parent.frame())
        }
        
        
},TRUE)#end try

if (class(tryIt)=="try-error"){#if an error occured
  if(ErrorOccured=="no"){
    errorOccurred("addVars.R",batch_mode)
  }

}else{#if no error
  
}#end if error

}#test if previous error
}#end function
  
