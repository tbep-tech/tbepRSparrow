#'@title diagnosticSensitivity
#'@description Test parameter sensitivity to 1% change.  
#'Outputs ~/estimate/(run_id)_diagnostic_sensitivity.pdf 
#'Uses subroutines: predictSensitivity,  errorOccurred.
#'@param path_results path to results directory
#'@param file_sum user specified run_id 
#'@param classvar character vector of user specified spatially contiguous discrete classification variables from sparrow_control
#'@param estimate.list Named list output of estimate.R subroutine containing objects: sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list, vANOVA.list,vMdiagnostics.list
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from sparrow_control
#'@param subdata input data (subdata)
#'@param sitedata.demtarea.class Total drainage area classification variable for calibration sites. Calculated by `calcDemtareaClass(sitedata$demtarea,batch_mode,ErrorOccured)`
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#
# diagnosticSensitivity.R
#
###############################################

diagnosticSensitivity <- function(path_results,file_sum,classvar,estimate.list,DataMatrix.list,SelParmValues,
                                  reach_decay_specification,reservoir_decay_specification,subdata,sitedata.demtarea.class,
                                  batch_mode,ErrorOccured) {
  
  # INPUT objects:
  
  if (ErrorOccured=="no"){
    tryIt<-try({  

  ####################################################################
  # create global variable from list names (JacobResults)
  for(i in 1:length(estimate.list$JacobResults)){
    tempobj=estimate.list$JacobResults[[i]]
    eval(parse(text=paste(names(estimate.list$JacobResults)[[i]],"= tempobj")))
  }
  # create global variable from list names (SelParmValues)
  for(i in 1:length(SelParmValues)){
    tempobj=SelParmValues[[i]]
    eval(parse(text=paste(names(SelParmValues)[[i]],"= tempobj")))
  }
  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(sitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(sitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  }
  depvar <- subdata$depvar
  xclass <- eval(parse(text=paste("subdata$",classrchvar,sep="")))
  ####################################################################
  
  filename <- paste(path_results,"/estimate/",file_sum,"_diagnostic_sensitivity.pdf",sep="")
  pdf(file=filename)
  
  
  # required SPARROW estimated coefficients (oEstimate, Parmnames)
  Estimate <- oEstimate  # initial baseline estimates
 
  # obtain baseline predictions all reaches
  predict <- predictSensitivity(Estimate,estimate.list,DataMatrix.list,SelParmValues,
                               reach_decay_specification,reservoir_decay_specification,subdata,batch_mode,ErrorOccured)

  apredict <- predict
  apredict_sum <- matrix(1,nrow=length(depvar),ncol=length(Estimate))
 
  ct <- length(Estimate)
  xiqr <- matrix(0,nrow=4,ncol=sum(ct))
  xmed <- numeric(sum(ct))
  xparm <- character(sum(ct))
  xvalue2 <- numeric(sum(ct))
  xsens <- matrix(0,nrow=sum(depvar > 0),ncol=length(Estimate))

  for (i in 1:length(Estimate)) {
   if(betaconstant[i] == 0) {     # an estimated parameter
  #  adjust parameter by 1%
     AEstimate <- Estimate
     AEstimate[i] <- Estimate[i] * 0.99
     apredict <- predictSensitivity(AEstimate,estimate.list,DataMatrix.list,SelParmValues,
                                   reach_decay_specification,reservoir_decay_specification,subdata,batch_mode,ErrorOccured)
     apredict_sum[,i] <- abs((apredict-predict)/predict*100) / 1.0  # change relative to 1% change
   }
  }

   # select site sensitivities 
   #   (only plot for site locations rather than nreach to reduce size of display)
   j<-0
   par(mfrow=c(2,2), pch=16)  # 4 plots on one page
   for (i in 1:length(Estimate)) {
     x1 <- apredict_sum[,i]
     xx <- data.frame(x1,depvar,xclass)
     parmsens <- xx[(xx$depvar > 0), ] 
     boxplot(parmsens$x1 ~ parmsens$xclass,xlab=classvar[1],ylab="Prediction Change (%) Relative to 1% Change",las = 2)
     title(bquote(paste("Parameter Sensitivity:  ",.(Parmnames[i]))))  
     xvalue2[i] <- i
     xiqr[,i] <- quantile(parmsens$x1, c(0.10,0.25,0.75,0.90)) 
     xmed[i] <- median(parmsens$x1)
     xparm[i] <- Parmnames[i]
     xsens[,i] <- parmsens$x1   # sensitivites for all calibration sites
   }

   # save results to directory and global environment
   sensitivities.list <- named.list(xparm,xmed,xiqr,xsens)
   objfile <- paste(path_results,"/estimate/",file_sum,"_sensitivities.list",sep="") 
   save(sensitivities.list,file=objfile)
   assign("sensitivities.list",sensitivities.list,envir = .GlobalEnv)
   
  # Plot median and IQR for each parameter
  xx <- xiqr[1,(xiqr[1,]>0)]  
  xminimum <- min(xx)
  xmed <- ifelse( xmed == 0,xminimum,xmed)
  xiqr <- ifelse( xiqr == 0,xminimum,xiqr)

  xupper <- xiqr[3,] - xmed
  xlower <- xmed - xiqr[2,]
  supper <- xiqr[4,] - xmed
  slower <- xmed - xiqr[1,]

  xupper <- ifelse(xupper == 0,xminimum,xupper)
  supper <- ifelse(supper == 0,xminimum,supper)
  xlower <- ifelse(xlower == 0,xminimum,xlower)
  slower <- ifelse(slower == 0,xminimum,slower)

  xx <- data.frame(xmed,xlower,xupper,supper,slower,xparm)
  xx <- xx[with(xx,order(xx$xmed)), ]  

  ymin <- min(xiqr)
  ymax <- max(xiqr)

# Arithmetic y axis
  par(mfrow=c(1,1), pch=16)    # 1 plots on one page
   plotCI(x=xvalue2, y=xx$xmed, uiw=xx$supper,liw=xx$slower,ylim=c(ymin,ymax),
    col="black",barcol="blue",pch=19,
    ylab="CHANGE IN PREDICTED VALUES (%)",xlab=" ",
    gap=0,
    las = 2,     # axis labels vertical
    xaxt = "n",   # Dont print x-axis
#    xlim=c(1,j)
    xlim=c(1,length(Estimate))
   )
#   axis(side=1, at=1:j, labels=xx$xparm, cex.axis=0.8, las = 2)
   axis(side=1, at=1:(length(Estimate)), labels=xx$xparm, cex.axis=0.8, las = 2)
   title("PARAMETER SENSITIVITY TO 1% CHANGE")

   plotCI(x=xvalue2, y=xx$xmed, uiw=xx$xupper,liw=xx$xlower,ylim=c(ymin,ymax),
    col="black",barcol="red",pch=19,
    gap=0,
    xaxt = "n",   # Dont print x-axis
    add=TRUE
   )

# Log y axis
  par(mfrow=c(1,1), pch=16)    # 1 plots on one page
   plotCI(x=xvalue2, y=xx$xmed, uiw=xx$supper,liw=xx$slower,ylim=c(ymin,ymax),
    col="black",barcol="blue",pch=19,log="y",
    ylab="CHANGE IN PREDICTED VALUES (%)",xlab=" ",
    gap=0,
    las = 2,     # axis labels vertical
    xaxt = "n",   # Dont print x-axis
#    xlim=c(1,j)
    xlim=c(1,length(Estimate))
   )
#   axis(side=1, at=1:j, labels=xx$xparm, cex.axis=0.8, las = 2)
   axis(side=1, at=1:(length(Estimate)), labels=xx$xparm, cex.axis=0.8, las = 2)
   title("PARAMETER SENSITIVITY TO 1% CHANGE")

   plotCI(x=xvalue2, y=xx$xmed, uiw=xx$xupper,liw=xx$xlower,ylim=c(ymin,ymax),
    col="black",barcol="red",pch=19,log="y",
    gap=0,
    xaxt = "n",   # Dont print x-axis
    add=TRUE
   )
   
   dev.off()  # shuts down current graphics device
   graphics.off()  # shuts down all open graphics devices
   
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("diagnosticSensitivity.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function

