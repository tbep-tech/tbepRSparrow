#
# estimateBootstraps.R

# Executes "if_boot_estimate" yes option

##########################################################

estimateBootstraps <- function(iseed,biters,estimate.list,path_results,file_sum,
                               csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured) {

 # INPUT objects:
 # estimate.list$HesResults
 # estimate.list$JacobResults
  
  # create global variable from list names (sparrowEsts)
#  for(i in 1:length(estimate.list$sparrowEsts)){
#    tempobj=estimate.list$sparrowEsts[[i]]
#    eval(parse(text=paste(names(estimate.list$sparrowEsts)[[i]],"= tempobj")))
#  }
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  # create global variable from list names (JacobResults)
  for(i in 1:length(estimate.list$JacobResults)){
    tempobj=estimate.list$JacobResults[[i]]
    eval(parse(text=paste(names(estimate.list$JacobResults)[[i]],"= tempobj")))
  }
  
  
  # transfer required variables to global environment from SUBDATA
  datalstCheck <- as.character(getVarList()$varList)
  for (i in 1:length(datalstCheck)) {
    dname <- paste("subdata$",datalstCheck[i],sep="") 
    x1name <- paste(datalstCheck[i],sep="")
    if((x1name %in% names(subdata)) == TRUE) {
      assign(datalstCheck[i],eval(parse(text=dname)))
    }
  }
  
  # execute parametric Monte Carlo sampling of model coefficients
  set.seed(iseed)
 
  # Check for esttype=="Estimated" coefficients and scaled parameters to setup up parameter vector
  #   to be consistent with Hessian covariance matrix dimensions
  
  # esttype <- estimate.list$JacobResults$esttype
  # bscale <- estimate.list$JacobResults$bscale
  # oEstimate <- estimate.list$JacobResults$oEstimate
  # btype <- estimate.list$JacobResults$btype
  
  k<-0
  ndim <- sum(ifelse(esttype=="Estimated",1,0))
  sEstimate <- numeric(ndim)
  stype <- numeric(ndim)
  sMEstimate <- matrix(0,nrow=biters,ncol=ndim)
  for (i in 1:length(oEstimate)) {
    if(esttype[i]=="Estimated") {
      k<-k+1
      stype[k] <- btype[i]
      if(if_auto_scaling == "yes") {
        sEstimate[k] <- oEstimate[i] * bscale[i]
      } else {
        sEstimate[k] <- oEstimate[i]
      }
    }
  }
  
  # Obtain coefficients with uncertainties that reflect variance-covariance between all coefficients
  # Coefficients are for thinned set of parameters, as required for estimateFeval.R
  #require(gear)
  cov2 <- estimate.list$HesResults$cov2   # coefficient variance from Hessian
  H <- decomp.cov(cov2) # decomp.cov returns a decomposition matrix H such that V <- H %*% t(H)
  sMestimate <- matrix(0,nrow=biters,ncol=ndim)
  for (i in 1:biters) {
     sMestimate[i,] <- sEstimate + H %*% rnorm(length(sEstimate),mean=0,sd=1)
  
    # Ensure that the coefficients are consistent with the parameter constraints
    #    bmin <- estimate.list$JacobResults$bmin
    #    bmax <- estimate.list$JacobResults$bmax    
    sMestimate[i,] <- pmax(bmin,sMestimate[i,])
    sMestimate[i,] <- pmin(bmax,sMestimate[i,])
  }

  # Remap unscaled coefficients to full set of Estimated and Fixed coefficients,
  #  in form suitable for calculating predictions
  k<-0
  bEstimate <- matrix(0,nrow=biters,ncol=length(oEstimate))
  for (i in 1:length(oEstimate)) {
    if(esttype[i]=="Estimated") {
      k<-k+1
      if(if_auto_scaling == "yes") {
        bEstimate[,i] <- sMestimate[,k] / bscale[i] # unscale estimate to support prediction
      } else {
        bEstimate[,i] <- sMestimate[,k]
      }
    } else { 
      # transfer "Fixed" coefficients
      bEstimate[,i] <- oEstimate[i]
    }
  }
  
# Run boot iterations to obtain leverage and 'mean_exp_weighted_error' for "Estimated" coefficients
  bootmean_exp_weighted_error <- numeric(biters)
 
  jacfun<-function(beta0,batch_mode,ErrorOccured) {
    Jac<-jacobian(estimateFeval,beta0,batch_mode=batch_mode,ErrorOccured=ErrorOccured,method="Richardson")
  }

  BetaEst <- numeric(length(bEstimate))
  Resids <- matrix(0,nrow=biters,ncol=length(estimate.list$sparrowEsts$resid))
  boot_resids <- matrix(0,nrow=biters,ncol=length(estimate.list$sparrowEsts$resid))
  boot_lev <- matrix(0,nrow=biters,ncol=length(estimate.list$sparrowEsts$resid))
  for (iter in 1:biters) {
    message(" Iteration ",iter)

    BetaEst <- sMestimate[iter,]  # Thinned ("Estimated") coefficients (also scaled) as required for estimateFeval.R
    Jacob <- jacfun(BetaEst,batch_mode,ErrorOccured)

     Jacob_inv <- solve(t(Jacob) %*% Jacob)  
     hlev <- rowSums( Jacob %*% Jacob_inv * Jacob)

     Resids[iter,] <- estimateFeval(BetaEst,batch_mode,ErrorOccured)  # executed with monitoring load adjustment and scaled thinned set of parameters
     boot_resid <- Resids[iter,] / sqrt(1 - hlev)
     boot_resids[iter,] <- boot_resid
     boot_lev[iter,] <- hlev
     if(max(hlev)>1) {
       bootmean_exp_weighted_error[iter] <- estimate.list$JacobResults$mean_exp_weighted_error
     } else {
       boot_weights <- rep(1,length(hlev))
       bootmean_exp_weighted_error[iter] <- (t(boot_weights) %*% exp(boot_resid)) / sum(boot_weights) 
     }
  }
  
 # store bootstrap parameter estimates in object as list and save

   BootResults <- named.list(bEstimate,bootmean_exp_weighted_error,boot_resids,boot_lev)
   objfile <- paste(path_results,"estimate/",file_sum,"_BootBetaest",sep="")
   save(BootResults,file=objfile)
   assign("BootResults",BootResults,envir = .GlobalEnv)
   
   # output to CSV
   outvars <- data.frame(rep(1:biters),bEstimate,bootmean_exp_weighted_error)
   headlist <- character(length(Parmnames)+2)
   headlist[1] <- "biters"
   for (i in 1:length(Parmnames)){
     headlist[i+1] <- Parmnames[i]
   }
   headlist[i+2] <- "bootmean_exp_weighted_error"
   colnames(outvars) <- headlist
   fileout <- paste(path_results,"estimate/",file_sum,"_bootbetaest.csv",sep="")
   fwrite(outvars,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
          dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("estimateBootstraps.R",batch_mode)
      }
    }else{#if no error
      return(BootResults)
    }#end if error
    
  }#test if previous error
}#end function
