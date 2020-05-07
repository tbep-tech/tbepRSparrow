#######################################################################################################
# OPTIMIZATION:

####################################################

 estimateOptimize <- function(path_results,file_sum,SelParmValues,estimate.input.list,DataMatrixEstimate.list
                              ,batch_mode,ErrorOccured){

   # INPUT:
   # SelParmValues
   # estimate.input.list
   # DataMatrixEstimate.list
   if (ErrorOccured=="no"){
     tryIt<-try({ 

   s_offset <- estimate.input.list$s_offset
   if_auto_scaling <- estimate.input.list$if_auto_scaling
   if_mean_adjust_delivery_vars <- estimate.input.list$if_mean_adjust_delivery_vars
   
   # Provide initial values for BETA0 for use in optimization (identify parameters to be estimated)
   ebcols <- SelParmValues$bcols-(sum(SelParmValues$betaconstant))  # number of parameters for estimation
   beta0 <- numeric(ebcols)
   betalst <- numeric(ebcols)
   betamn <- numeric(ebcols)
   betamx <- numeric(ebcols)
   k <- 0 
   # check min/max values to ensure that they bound initial value
     for (i in 1:SelParmValues$bcols) {
       if(SelParmValues$betaconstant[i] == 0) {
         k <- k+1
         beta0[k] <- DataMatrixEstimate.list$beta[1,i]
         betamn[k] <- SelParmValues$betamin[i]
         if(betamn[k] > beta0[k]) {
           betamn[k] <- beta0[k] / 10
         }
         betamx[k] <- SelParmValues$betamax[i]
         if(betamx[k] < beta0[k]) {
           betamx[k] <- beta0[k] * 10
         }
       }
     }

   
   filename <- paste(path_results,"/estimate/",file_sum,"_log.txt",sep="")
   sink(file=filename,split="TRUE")
   
    ptm <- proc.time()
   
    jacfun<-function(beta0,batch_mode,ErrorOccured) {
      Jac<-jacobian(estimateFeval,beta0,batch_mode=batch_mode,ErrorOccured=ErrorOccured,method="Richardson")
    }
    sparrowEsts <- nlfb(beta0,estimateFeval,batch_mode=batch_mode,ErrorOccured=ErrorOccured,jacfun,trace=TRUE,lower=betamn,upper=betamx,
                   control=list(offset=s_offset,ndstep=1e-07))

    # femax = Maximum function (sum of squares) evaluations. Default is 10000, which is extremely aggressive.
    # offset = Shift to test for floating-point equality. Default is 100.
    # jemax = Maximum number of Jacobian evaluations. Default is 5000.
    # ndstep = Stepsize to use to computer numerical Jacobian approximatin. Default is 1e-7.
    
    xtime <- proc.time() - ptm
    cat("\n \n")
    cat("Time elapsed in optimization\n ")
    print(xtime)
    cat("\n \n")
    
  sink(type="message")
  sink()
  
  # create unscaled original parameters
  sparrowEsts$if_auto_scaling <- if_auto_scaling
  if(if_auto_scaling == "yes") {
    sparrowEsts$coefficientsUnscaled <- sparrowEsts$coefficients / SelParmValues$pscale[SelParmValues$betaconstant==0]
    sparrowEsts$betamn <- betamn
    sparrowEsts$betamx <- betamx
  } else {
    sparrowEsts$coefficientsUnscaled <- sparrowEsts$coefficients
    sparrowEsts$betamn <- betamn
    sparrowEsts$betamx <- betamx
  }
  sparrowEsts$if_mean_adjust_delivery_vars <- if_mean_adjust_delivery_vars
    
  # SAVE SPARROW MODEL OBJECT TO FILE
  #  (Use 'load' command to read object into R
  objfile <- paste(path_results,"/estimate/",file_sum,"_sparrowEsts",sep="")
  save(sparrowEsts,file=objfile)
  
  
  
     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured

       sink()
       if(ErrorOccured=="no"){
         errorOccurred("estimateOptimize.R",batch_mode)
       }
     }else{#if no error
       return(sparrowEsts)
     }#end if error
     
   }#test if previous error
 }#end function