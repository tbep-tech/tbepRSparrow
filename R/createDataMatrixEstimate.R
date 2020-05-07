#'@title createDataMatrixEstimate
#'@description Function to create DATA matrix and populate with data1 values from subdata 
#'Uses subroutines: getVarList, named.list, errorOccurred.
#'@param if_auto_scaling yes/no character string specifying if auto-scaling of parameters for optimization is to be used
#'@param if_mean_adjust_delivery_vars yes/no character string indicating if the delivery variables are to be mean adjusted from sparrow_control
#'@param subdata input data (subdata) 
#'@param SelParmValues selected parameters from parameters.csv using condition `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`
#'@param df betavalues - list of parameters from parameters.csv
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return DataMatrixEstimate.list - a named list of data,beta,data.index.list


createDataMatrixEstimate <- function(if_auto_scaling,if_mean_adjust_delivery_vars,subdata,SelParmValues,df,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({

  # setup the 12 required variables
  datalst <- as.character(getVarList()$matrixlst)

  #########################################################
  # create global variable from list names
  for(i in 1:length(SelParmValues)){
    tempobj=SelParmValues[[i]]
    eval(parse(text=paste(names(SelParmValues)[[i]],"= tempobj")))
  }
  
  # transfer required variables to global environment from SUBDATA
  master.list <- c(datalst,srcvar,dlvvar,decvar,resvar,othervar)
  for (i in 1:length(master.list)) {
    dname <- paste("subdata$",master.list[i],sep="") 
    x1name <- paste(master.list[i],sep="")
    if((x1name %in% names(subdata)) == TRUE) {
      assign(master.list[i],eval(parse(text=dname)))
    }
  }
  #########################################################
  # setup index values for DATA matrix by name 
  
  jwaterid <- which(datalst%in%"waterid") # SPARROW Reach Identifier
  jstaid <- which(datalst%in%"staid")     # SPARROW Monitoring Station Identifier 
  jfnode <- which(datalst%in%"fnode")     # Upstream Reach Node Identifier
  jtnode <- which(datalst%in%"tnode")     # Downstream Reach Node Identifier 
  jfrac <- which(datalst%in%"frac")       # Fraction Upstream Flux Diverted to Reach
  jiftran <- which(datalst%in%"iftran")   # If Reach Transmits Flux (1=yes, 0=no)
  jtarget <- which(datalst%in%"target")   # Downstream target reach
  jtotarea <- which(datalst%in%"demtarea")# Total upstream drainage area (km2)
  jiarea <- which(datalst%in%"demiarea")  # Incremental reach drainage area (km2)
  jdepvar <- which(datalst%in%"depvar")   # Dependent variable load (kg/yr)
  jhydseq <- which(datalst%in%"hydseq")   # SPARROW Reach Hydrologic Sequencing Code
  jmean_flow <- which(datalst%in%"meanq") # Mean flow (cfs)
  
  ivar <- 12   # number of required network variables
  
  # pselect created in 'selectParmValues.R'
  srcselect <- ifelse(df$parmType == "SOURCE" & pselect == 1,1,0)
  dlvselect <- ifelse(df$parmType == "DELIVF" & pselect == 1,1,0)
  decselect <- ifelse(df$parmType == "STRM" & pselect == 1,1,0)
  resselect <- ifelse(df$parmType == "RESV" & pselect == 1,1,0)
  otherselect <- ifelse(df$parmType == "OTHER" & pselect == 1,1,0)
  
  # set index values for variable types selected
  if (sum(srcselect) > 0) {
    jbsrcvar <- rep(1:sum(srcselect),1)
    jsrcvar <- jbsrcvar + ivar
  } else {
    jbsrcvar <- 0
    jsrcvar <- 0
  }
  if (sum(dlvselect) > 0) {
    jbdlvvar <- rep(max(jbsrcvar)+1:sum(dlvselect),1)
    jdlvvar <- jbdlvvar + ivar
  } else {
    jbdlvvar <- 0
    jdlvvar <- 0
  }
  if (sum(decselect) > 0) {
    jbdecvar <- rep((max(jbsrcvar)+sum(dlvselect))+1:sum(decselect),1)
    jdecvar <- jbdecvar + ivar
  } else {
    jbdecvar <- 0
    jdecvar <- 0
  }
  if (sum(resselect) > 0) {
    jbresvar <- rep((max(jbsrcvar)+sum(dlvselect)+sum(decselect))+1:sum(resselect),1)
    jresvar <- jbresvar + ivar
  } else {
    jbresvar <- 0
    jresvar <- 0
  }
  
  if (sum(otherselect) > 0) {
    jbothervar <- rep((max(jbsrcvar)+sum(dlvselect)+sum(decselect)+sum(resselect))+1:sum(otherselect),1)
    jothervar <- jbothervar + ivar
  } else {
    jbothervar <- 0
    jothervar <- 0
  }
  data.index.list <- named.list(jwaterid,jstaid,jfnode,jtnode,jfrac,jiftran,jtarget,
                                  jtotarea,jiarea,jdepvar,jhydseq,jmean_flow,
                                  jsrcvar,jdlvvar,jdecvar,jresvar,jothervar,
                                  jbsrcvar,jbdlvvar,jbdecvar,jbresvar,jbothervar,
                                  if_mean_adjust_delivery_vars,if_auto_scaling)

  ######################################################
  # transfer data from vectors to 'DATA' matrix
  ncols <- ivar + bcols
  data <- matrix(1:length(depvar), ncol=ncols, nrow=length(depvar))

  for (i in 1:ivar) {
    data[,i] <- eval(parse(text=datalst[i]))  # transfer required 12 variables to data
  }
  
  # Rescale DATA and beta0 based on pscale factors
  #  pscale is the factor required to rescale the initial value between 1 and 10
  
  # transfer source variables
  iend <- ivar+length(srcvar)
  j<-0
  if(if_auto_scaling == "yes") {
    for (i in (ivar+1):iend) {
      j <- j+1
      data[,i] <- eval(parse(text=srcvar[j]))/pscale[j]

    }
  } else {
    for (i in (ivar+1):iend) {
      j <- j+1
      data[,i] <- eval(parse(text=srcvar[j]))
    }
  }

  # transfer delivery variables
  if(max(jdlvvar) != 0) {
    ibeg <- ivar+length(srcvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)
    j<-0
    k <- length(srcvar)  # starting k index for scale vector
    if(if_auto_scaling == "yes") {
      for (i in ibeg:iend) {
        j <- j+1
        k <- k+1
        if(if_mean_adjust_delivery_vars == "yes"){
          data[,i] <- eval(parse(text=dlvvar[j]))/pscale[k] - mean(eval(parse(text=dlvvar[j]))/pscale[k])
        }
        else {
          data[,i] <- eval(parse(text=dlvvar[j]))/pscale[k]
        }
      }
    } else {
      for (i in ibeg:iend) {
        j <- j+1
        if(if_mean_adjust_delivery_vars == "yes"){
          data[,i] <- eval(parse(text=dlvvar[j])) - mean(eval(parse(text=dlvvar[j])))
        }
        else {
          data[,i] <- eval(parse(text=dlvvar[j]))
        }
      }
    }
  } # end length check

  # transfer reach decay variables
  if(max(jdecvar) != 0) {
    ibeg <- ivar+length(srcvar)+length(dlvvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)+length(decvar)
    j<-0
    k <- length(srcvar)+length(dlvvar)
    if(if_auto_scaling == "yes") {
      for (i in ibeg:iend) {
        j <- j+1
        k <- k+1
        data[,i] <- eval(parse(text=decvar[j]))/pscale[k]
      }
    } else {
      for (i in ibeg:iend) {
        j <- j+1
        data[,i] <- eval(parse(text=decvar[j]))
      }
    }


  } # end length check

  # transfer reservoir decay variables
  if(max(jresvar) != 0) {
    ibeg <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+length(resvar)
    j<-0
    k <- length(srcvar)+length(dlvvar)+length(decvar)
    if(if_auto_scaling == "yes") {
      for (i in ibeg:iend) {
        j <- j+1
        k <- k+1
        data[,i] <- eval(parse(text=resvar[j]))/pscale[k]
      }
    } else {
      for (i in ibeg:iend) {
        j <- j+1
        data[,i] <- eval(parse(text=resvar[j]))
      }
    }
  } # end length check

  # transfer other variables
  if(max(jothervar) != 0) {
    ibeg <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+length(resvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+length(resvar)+length(othervar)
    j<-0
    k <- length(srcvar)+length(dlvvar)+length(decvar)+length(resvar)
    if(if_auto_scaling == "yes") {
      for (i in ibeg:iend) {
        j <- j+1
        k <- k+1
        data[,i] <- eval(parse(text=othervar[j]))/pscale[k]
      }
    } else {
      for (i in ibeg:iend) {
        j <- j+1
        data[,i] <- eval(parse(text=othervar[j]))
      }
    }
  } # end length check
  
  beta <- matrix(1:length(depvar), ncol=bcols, nrow=length(depvar))
  if(if_auto_scaling == "yes") {
    for (i in 1:bcols) {
      beta[,i] <- beta0[i]*pscale[i]
    }
  } else {
    for (i in 1:bcols) {
      beta[,i] <- beta0[i]
    }
  }


  DataMatrixEstimate.list <- named.list(data,beta,data.index.list)
 
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("createDataMatrixEstimate.R",batch_mode)
      }
    }else{#if no error
 return(DataMatrixEstimate.list)
    }#end if error
    
  }#test if previous error
}#end function

