#'@title checkClassificationVars
#'@description Checks for missing or zero values in the classification variables, which causes a critical error and program shutdown
#'Uses subroutines: errorOccurred. 
#'@param subdata input data (subdata) 
#'@param classvar character vector of user specified spatially contiguous discrete classification variables from sparrow_control
#'@param class_landuse character vector of class_landuses from sparrow_control
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`

checkClassificationVars<-function(subdata,classvar,class_landuse,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({
 
      if (!is.na(classvar)){
      #test for class var in names subdata
      testClassvar<-classvar[which(!classvar %in% names(subdata))]
      if (length(testClassvar)!=0){
        ErrorOccured<-"yes"
        message(paste0("INVALID classvar ",paste(testClassvar,collapse=", ")," NOT FOUND IN dataDictionary.csv \nRUN EXECUTION TERMINATED"))
        if (batch_mode=="yes"){#if batch output message to log
          cat("INVALID classvar ",paste(testClassvar,collapse=", ")," NOT FOUND IN dataDictionary.csv \nRUN EXECUTION TERMINATED",sep="")
        }
        assign("ErrorOccured","yes",envir = .GlobalEnv)
        assign("ErrorOccured","yes",envir = parent.frame())
        exit()
      }
      }
      
    #check that no NAs exist in the user definced classvar and class_landuse variables 
      colsOrder<-c(na.omit(c(classvar,class_landuse)))
      if (length(colsOrder)!=0){
  cols<-subdata[,which(names(subdata) %in% colsOrder)]
  cols<-cols[,match(colsOrder,names(cols))]
  for (c in 1:length(cols)){
    check<-cols[[c]]
    check<-check[which(is.na(check))]
    if (length(check)!=0){
      if (names(cols)[c] %in% classvar){
        type<-"classvar"
      }else{
        type<-"class_landuse"
      }
      cat("\n \n")
      message(paste("ERROR the following ",type," variable has MISSING values : ",names(cols)[c]))
      message(paste("\nMISSING VALUES IN 'classvar' AND/OR 'class_landuse' VARIABLES WILL CAUSE PROGRAM FAILURE!"))
      cat("\n \n")
      if (batch_mode=="yes"){
        cat("\n \n")
        cat(paste("ERROR the following ",type," variable has MISSING values : ",names(cols)[c]))
        cat(paste("\nMISSING VALUES IN 'classvar' AND/OR 'class_landuse' VARIABLES WILL CAUSE PROGRAM FAILURE!"))
        cat("\n \n") 
      }
    }
  }
      }
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("checkClassificationVars.R",batch_mode)
      }
    }else{#if no error
    }#end if error
    
  }#test if previous error
}#end function