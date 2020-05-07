#
# selectParmValues.R
#
##############################################
# Set selected beta coefficients (initial, minimum, maximum)
#   (nonzero maximum value indicates which subset of variables are to be estimated)

selectParmValues <- function(df,if_estimate,if_estimate_simulation,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

 pScaleFactor <- function(x) {
 # pScaleFun creates scaling factor for SPARROW input data
 # Purpose:  ensures consistency in value ranges for all parameters (scaled to fall between 1 and 10)
 # x = initial expected SPARROW model coefficients (beta0)
 # returned value = scaling factor
 # NOTE: the input is scaled to equal the product of unscaled input and reciprocal scaling factor
    b <- c(1e-10,1e-09,1e-08,1e-07,1e-06,1e-05,1e-04,1e-03,1e-02,1e-01,1,1e+01,1e+02,1e+03,1e+04,1e+05,1e+06,
           1e+07,1e+08,1e+09,1e+10)
    pscale <- numeric(length(x))
    for (k in 1:length(x)) {
     for (i in 1:(length(b)-1)) {
       if(abs(x[k])>=b[i] & abs(x[k])<b[i+1]) {pscale[k] <- 1/b[i]}
     }
    }
    return(pscale)
 }

 pselect <- ifelse(df$parmConstant==1 | (df$parmMax > 0 | (df$parmType=="DELIVF" & df$parmMax>=0)) & 
                     (df$parmMin<df$parmMax) & 
                     ((df$parmType=="SOURCE" & df$parmMin>=0) | df$parmType!="SOURCE")
                   ,1,0)   # identify selected variables
 srcselect <- ifelse(df$parmType == "SOURCE" & pselect == 1,1,0)
 dlvselect <- ifelse(df$parmType == "DELIVF" & pselect == 1,1,0)
 decselect <- ifelse(df$parmType == "STRM" & pselect == 1,1,0)
 resselect <- ifelse(df$parmType == "RESV" & pselect == 1,1,0)
 otherselect <- ifelse(df$parmType == "OTHER" & pselect == 1,1,0)
 bcols <- sum(pselect)

# transfer parameters for selected variables
 beta0 <- df$parmInit[pselect == 1]
 betamin <- df$parmMin[pselect == 1]
 betamax <- df$parmMax[pselect == 1]
 betatype <- df$parmType[pselect == 1]
 betaconstant <- df$parmConstant[pselect == 1]
 bsrcconstant <- df$parmConstant[df$parmType == "SOURCE" & pselect == 1]
 bCorrGroup <- df$parmCorrGroup[pselect == 1]
 sparrowNames <- df$sparrowNames[pselect == 1]

# set scaling parameters (default setting to values in parameters.csv)
 pscale <- df$parmScale[pselect == 1]

 if(sum(pscale) == length(pscale)) {
 # Use auto-scaling algorithm as default (all CSV table scale values = 1) 
    pscale <- pScaleFactor(beta0)  # auto scaling
    bsrcscale <- pscale
 } else {
 # Use manual scaling based on user-specified values in parameter CSV file
    bsrcscale <- pscale
 }

 srcvar <- df$sparrowNames[srcselect == 1]
 dlvvar <- df$sparrowNames[dlvselect == 1]
 decvar <- df$sparrowNames[decselect == 1]
 resvar <- df$sparrowNames[resselect == 1]
 othervar <- df$sparrowNames[otherselect == 1]
 

# NOTE: only pscale is used subsequently to scale data and parameters
#       bsrcscale is used in estimateNLLSmetrics.R to set values for output and objects
#       consider deleting bsrcscale and using pscale only

 SelParmValues <- named.list(sparrowNames,bcols,beta0,betamin,betamax,pscale,betatype,pselect,
                             betaconstant,bsrcconstant,bsrcscale,bCorrGroup,
                             srcvar,dlvvar,decvar,resvar,othervar)
 
 #checks on parameter values
 exit <- function() {
   .Internal(.invokeRestart(list(NULL, NULL), NULL))
 }
 
 npar <- bcols-(sum(betaconstant)) 
if (length(betamin)!=bcols){
  #wrong length lower
  if (if_estimate=="yes" | if_estimate_simulation=="yes"){
    ErrorOccured<-"yes"
    message("INVALID NUMBER OF parmMin VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMin VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
    if (batch_mode=="yes"){#if batch output message to log
      cat("INVALID NUMBER OF parmMin VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMin VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
    }
    assign("ErrorOccured","yes",envir = .GlobalEnv)
    assign("ErrorOccured","yes",envir = parent.frame())
    exit()
  }
}
 if (length(betamax)!=bcols){
   #wrong length upper
   if (if_estimate=="yes" | if_estimate_simulation=="yes"){
     ErrorOccured<-"yes"
     message("INVALID NUMBER OF parmMax VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMax VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     if (batch_mode=="yes"){#if batch output message to log
       cat("INVALID NUMBER OF parmMax VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMax VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     }
     assign("ErrorOccured","yes",envir = .GlobalEnv)
     assign("ErrorOccured","yes",envir = parent.frame())
     exit()
   }
 }
 if (any(beta0<betamin)){
   #bad start too small
  if (if_estimate=="yes" | if_estimate_simulation=="yes"){
   ErrorOccured<-"yes"
   message("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit>=parmMin \nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
   if (batch_mode=="yes"){#if batch output message to log
     cat("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit>=parmMin\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
   }
   assign("ErrorOccured","yes",envir = .GlobalEnv)
   assign("ErrorOccured","yes",envir = parent.frame())
   exit()
  }
 }
 if (any(beta0>betamax)){
   #bad start too big
   if (if_estimate=="yes" | if_estimate_simulation=="yes"){
     ErrorOccured<-"yes"
     message("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit<=parmMax \nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     if (batch_mode=="yes"){#if batch output message to log
       cat("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit<=parmMax\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     }
     assign("ErrorOccured","yes",envir = .GlobalEnv)
     assign("ErrorOccured","yes",envir = parent.frame())
     exit()
   }
 }
 

 
 
 if (any(betamin>betamax)){
   #min>max
   if (if_estimate=="yes" | if_estimate_simulation=="yes"){
     ErrorOccured<-"yes"
     message("INVALID parmMin/parmMax VALUES FOUND IN PARAMETERS FILE.\nparmMin MUST BE LESS THAN parmMax\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     if (batch_mode=="yes"){#if batch output message to log
       cat("INVALID parmMin/parmMax VALUES FOUND IN PARAMETERS FILE.\nparmMin MUST BE LESS THAN parmMax\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     }
     assign("ErrorOccured","yes",envir = .GlobalEnv)
     assign("ErrorOccured","yes",envir = parent.frame())
     exit()
   }
 }
 if (all(beta0==0)){
   #parmInit == 0
   if (if_estimate=="yes" | if_estimate_simulation=="yes"){
     ErrorOccured<-"yes"
     message("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST NOT EQUAL ZERO FOR SELECTED PARAMETERS\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     if (batch_mode=="yes"){#if batch output message to log
       cat("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST NOT EQUAL ZERO FOR SELECTED PARAMETERS\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
     }
     assign("ErrorOccured","yes",envir = .GlobalEnv)
     assign("ErrorOccured","yes",envir = parent.frame())
     exit()
   }
 }

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("selectParmValues.R",batch_mode)
      }
    }else{#if no error
 return(SelParmValues)
    }#end if error
    
  }#test if previous error
}#end function

