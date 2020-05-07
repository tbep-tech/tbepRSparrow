#'@title estimateWeightedErrors
#'@description Estimate the error (residual) weights for a SPARROW weighted NLLS, based on errors from prior model run
#'Uses subroutines: read.csv,errorOccurred, lm, nls, min, max.
#'@param path_user path to User directory (e.g., above 'results' subdirectory)
#'@param results_directoryName path to the user 'results' directory 
#'@param pre_run_id the run_id for the prior model residuals
#'@param nreaches number of reaches
#'@param calsites calibration site indicator (NA or 0 = not selected; 1=selected)
#'@param csv_columnSeparator user-designated column separator
#'@param csv_decimalSeparator user-designated decimal separator
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return weight nonlinear regression estimates of the error weights (length=number of reaches)
#
# estimateWeightedErrors.R
#
##############################################
# Estimate the error (residual) weights for a SPARROW weighted NLLS, based on errors from prior model run

estimateWeightedErrors <- function(path_user,results_directoryName,pre_run_id,nreaches,calsites,
                                   csv_columnSeparator,csv_decimalSeparator,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 
      
      # predicted weights developed per method in equation 1.50 (Schwarz et al. 2006) 
      #  using nonlinear power function regression of squared log residuals on log of predicted loads
      
      # read the _residuals.csv file contents from a prior model run (sites in downstream order)
      filename <- paste(path_user,"/",results_directoryName,"/",pre_run_id,"/estimate/",pre_run_id,"_residuals.csv",sep="")
      Ctype <- c("numeric","numeric","character","character","character","numeric","numeric","numeric","numeric","numeric",
                 "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
      NAMES<- c("waterid","demtarea","rchname","station_id","station_name","staid","sitedata.demtarea.class","Obs","predict","Obsyield",
                "predictYield","Resids","standardResids","hlev","boot_resid","weight","tiareakm2","pResids")
      numberFields<-max(count.fields(filename,sep=csv_columnSeparator))
      oResids <- read.csv(filename,header=TRUE,colClasses=Ctype,
                          dec = csv_decimalSeparator,sep=csv_columnSeparator)
      
      # linear regression weights
      y <- oResids$Resids^2
      x <- log(oResids$predict)
      reg <- lm(y ~ x)
      cmin <- min(reg$fitted.values[reg$fitted.values>0])
      reg$fitted.values <- ifelse(reg$fitted.values>0,reg$fitted.values,cmin)
      weights_lr <- 1/reg$fitted.values     # returned reciprocal variance
      # summary(1/reg$fitted.values)          # reciprocal variance
      # summary(sqrt(1/reg$fitted.values))    # weight applied in feval
      # summary(reg)
      # plot(x,y)
      # lines(x,reg$fitted.values)
      
      # nonlinear regression weights
      inits <- numeric(2)
      inits[1] <- 1
      inits[2] <- 1
      sqResids <- y
      lnload <-x
      nls.st <- c(a = inits[1], b1 = inits[2])
      regnls <- nls(sqResids ~ a * lnload**b1,start = nls.st)
      Estimate <- as.double(coef(regnls))
      weights_nlr <- 1/(Estimate[1] * x**Estimate[2]) # returned reciprocal variance
      
      NLLS_weights <- "lnload"
      Csites.weights.lnload.list <- named.list(NLLS_weights,weights_nlr,sqResids,lnload,regnls)
      assign("Csites.weights.lnload.list",Csites.weights.lnload.list,envir = .GlobalEnv)
      
#      summary(regnls)
      xy <- data.frame(x,y)
      xy <- xy[order(x),]
      plot(xy$x,xy$y,ylab="Squared Log Residuals",xlab="Log Predicted Load",main="NLLS Weight Function")
      NLS_predict <- Estimate[1] * xy$x**Estimate[2]
      lines(xy$x,NLS_predict,col="red")
#      summary(1/NLS_predict)          # reciprocal variance
#      summary(sqrt(1/NLS_predict))    # weight applied in feval
      
      # transfer weights_nlr (length=number calibration sites) to weight (length=number reaches)
      weight <- numeric(nreaches)
      calsites <- ifelse(is.na(calsites),0,calsites)
      weight[calsites==1] <- weights_nlr
      
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("estimateWeightedErrors.R",batch_mode)
      }
    }else{#if no error
 return(weight)  # return the nonlinear weights
    }#end if error
    
  }#test if previous error
}#end function

