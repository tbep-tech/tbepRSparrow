#
# readDesignMatrix.R
#
###########################################################
# READ SOURCE-DELIVERY DESIGN MATRIX
###########################################################

readDesignMatrix <- function(path_results,file_design,betavalues, csv_decimalSeparator, csv_columnSeparator,
                             batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  filed <- paste(path_results,file_design,"_design_matrix.csv",sep="")
  
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
    exit <- function() {
      .Internal(.invokeRestart(list(NULL, NULL), NULL))
    }
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
  
  #order according to parameters
  dmatrixin<-as.data.frame(matrix(dmatrixin[match(betavalues[which(betavalues$parmType=="SOURCE"),]$sparrowNames,rownames(dmatrixin)),],
                                  ncol=ncol(dmatrixin),nrow=nrow(dmatrixin),dimnames = list(rownames(dmatrixin),colnames(dmatrixin))))
  dmatrixin<-as.data.frame(matrix(dmatrixin[,match(betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames,names(dmatrixin))],
                                  ncol=ncol(dmatrixin),nrow=nrow(dmatrixin),dimnames = list(rownames(dmatrixin),colnames(dmatrixin))))

      },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("readDesignMatrix.R",batch_mode)
      }
    }else{#if no error
return(dmatrixin)
    }#end if error
    
  }#test if previous error
}#end function
