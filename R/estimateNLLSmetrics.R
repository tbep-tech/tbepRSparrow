#
# estimateNLLSmetrics.R
#
#################################################

estimateNLLSmetrics <- function(if_estimate,if_estimate_simulation,ifHess,if_auto_scaling,if_sparrowEsts,sparrowEsts,
                                path_results,file_sum,classvar,classrchvar,dlvdsgn,yieldFactor,
                                Csites.weights.list,
                                Csites.list,SelParmValues,subdata,sitedata,DataMatrix.list,batch_mode,ErrorOccured) {
  
  # INPUT objects
  # Csites.list
  # sparrowEsts
  # SelParmValues
  # subdata
  # sitedata
  # DataMatrix.list
  # dlvdsgn
  
  # OUTPUT objects (estimate.metrics.list):
  # JacobResults
  # HesResults
  # ANOVA.list
  # Mdiagnostics.list
  if (ErrorOccured=="no"){
    tryIt<-try({
      exit <- function() {
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }
      

  message("Computing NLLS metrics...")


  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(sitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(sitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  } 

  # contiguous class variables by reach
  classrch <- as.numeric(eval(parse(text=paste("subdata$",classrchvar,sep=""))))  # used to compute RMSE by class

  
  # Obtain predicted, observed, residual values
  nreach <- length(DataMatrix.list$data[,1])
  numsites <- Csites.list$nMon
  npar <- length(sparrowEsts$coefficients)
  mobs <- numsites
  Obs <- numeric(numsites)
  predict <- numeric(numsites)
  yldobs <- numeric(numsites)
  yldpredict <- numeric(numsites)
  xstaid <- numeric(numsites)
  tarea <- numeric(numsites)
  ratio.obs.pred <- numeric(numsites)
  xlat <- numeric(numsites)
  xlon <- numeric(numsites)

  
  data <- DataMatrix.list$data
  
  # create global variable from list names
  for(i in 1:length(SelParmValues)){
    tempobj=SelParmValues[[i]]
    eval(parse(text=paste(names(SelParmValues)[[i]],"= tempobj")))
  }
  # transfer required variables to global environment from 'DataMatrix.list$data.index.list'
  for(i in 1:length(DataMatrix.list$data.index.list)){
    tempobj=DataMatrix.list$data.index.list[[i]]
    eval(parse(text=paste(names(DataMatrix.list$data.index.list)[[i]],"= tempobj")))
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
  
  xssemrb <- numeric(max(class[,1]))

  Resids <- sparrowEsts$resid
  ins <- 0
  for (k in 1:nreach) {
    if (data[k,jdepvar] > 0) {
       ins <- ins+1
       obs <- data[k,jdepvar]
       Obs[ins] <- data[k,jdepvar]
       predict[ins] <- (exp(log(obs) - Resids[ins]))  
       yldobs[ins] <- Obs[ins] / data[k,jtotarea] * yieldFactor
       yldpredict[ins] <- predict[ins] / data[k,jtotarea] * yieldFactor
       xstaid[ins] <- staid[k]
       tarea[ins] <- data[k,jtotarea]
       ratio.obs.pred[ins] <- Obs[ins] / (predict[ins])
       xlat[ins] <- lat[k]
       xlon[ins] <- lon[k]

  # compute SSE by MRB
       if(class[ins,1] > 0) {
        xssemrb[class[ins,1]] <- xssemrb[class[ins,1]] + Resids[ins]^2
       }
    }                                 
  }  #  reach counter 
        xssemrb[is.na(xssemrb)] <- 0

################################################
# COMPUTE SUMMARY STATISTICS (ONLY ESTIMATION)

  SSE <- sum(Resids^2)
  DF <- mobs-npar
  MSE <- SSE / DF
  RMSE <- sqrt(MSE)

  RSQ <- 1 - SSE / (sum(log(Obs)^2) - sum(log(Obs))^2/mobs)
  RSQ_ADJ <- 1 - ((mobs - 1)/DF)*(1 - RSQ) 
  RSQ_YLD <- 1 - SSE / (sum(log(yldobs)^2) - sum(log(yldobs))^2/mobs)

  NSn <- 0
  NSd <- 0
  PBiasn <- 0
  PBiasd <- 0
  Obsmean <- mean(log(Obs))
   for (i in 1:mobs) {
     NSn <- NSn + (log(Obs[i]) - log(predict[i]))^2
     NSd <- NSd + (log(Obs[i]) - Obsmean)^2
     PBiasn <- PBiasn + (Obs[i] - predict[i])
     PBiasd <- PBiasd + Obs[i]
   }
   NSeff <- 1 - (NSn / NSd)    # overall Nash-Sutcliffe model efficiency
   PBias <- PBiasn / PBiasd * 100   # Percent bias

  ####################################################
  # Obtain SEs for coefficients using Jacobian
  Estimate <- sparrowEsts$coefficients                 # scaled coefficients 1-24-2017
  EstimateUnscaled <- sparrowEsts$coefficientsUnscaled
  SEj <- numeric(length(Estimate))  

if (if_estimate == "yes" & if_estimate_simulation == 'no') {

  # remove parameters that hit min or max boundary, or are too close to zero (added 7-29-2016)
  k <- 0
  for (i in 1:npar) {
#    if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i]) & abs(Estimate[i]) > 1.0e-04) { 
    if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i])) { 
      k <- k+1 
    } 
  } 
  if (k != npar) {
    tJTJ <- array(0,dim=c(k,k))
    tEstimate <- numeric(k)
    tjacobian <- array(0,dim=c(mobs,k))
    ii <- 0
    for (i in 1:npar) {
#      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i]) & abs(Estimate[i]) > 1.0e-04) {
      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i])) {
       ii <- ii+1
       tEstimate[ii] <- Estimate[i]
       tjacobian[,ii] <- sparrowEsts$jacobian[,i]
      }
    }
    tJTJ <- crossprod(tjacobian)   # crossproduct of jacobian and its transpose
    
    ErrorSolve <- grep("singular",try({covmat <- MSE * solve(tJTJ)},TRUE))
    if(length(ErrorSolve)>0) {
      if(ErrorSolve==1) {
        if(ErrorOccured=="no"){
          cat("\n \n")
          if (!file.exists(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.txt"))){
            message("SINGULAR (ILL-CONDITIONED) JACOBIAN MATRIX FOUND.
                    MODEL ESTIMATION SUMMARY METRICS ARE NOT OUTPUT. 
                    RUN EXECUTION TERMINATED.
                    
                    THIS CONDITION CAN BE CAUSED BY COLLINEARITIES IN TWO OR MORE EXPLANATORY
                    VARIABLES. PARAMETERS WITH SMALL, NEAR ZERO ESTIMATES MAY INDICATE THE 
                    PRESENCE OF COLLINEARITIES AMONG THESE OR OTHER PARAMETERS.
                    
                    USERS SHOULD ELIMINATE ONE OF THE COLLINEAR VARIABLES AND RE-ESTIMATE 
                    THE MODEL. TABULAR OUTPUT OF BIVARIATE CORRELATIONS FOR THE EXPLANATORY 
                    VARIABLES MAY POTENTIALLY TO IDENTIFY COLLINEAR VARIABLES. CONSIDER USE OF THE 'select_corr<-yes' 
                    SETTING TO EVALUATE COLLINEARITIES.

                    USERS SHOULD ALSO CHECK THE EXPLANATORY VARIABLES FOR CASES WHERE ALL OR
                    VIRTUALLY ALL REACHES HAVE ZERO VALUES. 
                    ")
          }else{
            message(paste0("SINGULAR (ILL-CONDITIONED) JACOBIAN MATRIX FOUND.
                           MODEL ESTIMATION SUMMARY METRICS ARE NOT OUTPUT. 
                           RUN EXECUTION TERMINATED.
                           
                           THIS CONDITION CAN BE CAUSED BY COLLINEARITIES IN TWO OR MORE EXPLANATORY
                           VARIABLES. PARAMETERS WITH SMALL, NEAR ZERO ESTIMATES MAY INDICATE THE 
                           PRESENCE OF COLLINEARITIES AMONG THESE OR OTHER PARAMETERS.
                           USERS SHOULD ELIMINATE ONE OF THE COLLINEAR VARIABLES AND RE-ESTIMATE 
                           THE MODEL. TABULAR OUTPUT OF BIVARIATE CORRELATIONS FOR THE EXPLANATORY 
                           VARIABLES MAY POTENTIALLY TO IDENTIFY COLLINEAR VARIABLES.  EVALUATE COLLINEARITIES USING 
                           OUTPUT FROM THE 'select_corr<-yes' METHOD : \n \n ",path_results,"estimate/", file_sum,"_explvars_correlations.pdf \n \n",
                           path_results,"estimate/", file_sum,"_explvars_correlations.txt\n \n
                           USERS SHOULD ALSO CHECK THE EXPLANATORY VARIABLES FOR CASES WHERE ALL OR
                           VIRTUALLY ALL REACHES HAVE ZERO VALUES." 
            ))
            shell.exec(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.txt"))
            shell.exec(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.pdf"))
          }
          ErrorOccured<-"yes"
        }
        assign("ErrorOccured","yes",envir = .GlobalEnv)
        assign("ErrorOccured","yes",envir = parent.frame())
        Message <- "ERROR"
        JacobResults <- named.list(Message)
        estimate.metrics.list <- named.list(JacobResults)
        #return(estimate.metrics.list)
        exit()
      }
      }
    
   # covmat <- MSE * solve(tJTJ)
    tSEj <- sqrt(diag(covmat))

    ii <- 0
    for (i in 1:npar) {
#      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i]) & abs(Estimate[i]) > 1.0e-04)  {
      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i]))  {
       ii <- ii+1
       SEj[i] <- tSEj[ii]
      } else {
       SEj[i] <- 0
      }
    } 
  } else {
    JTJ <- crossprod(sparrowEsts$jacobian)   # crossproduct of jacobian and its transpose
    
    ErrorSolve <- grep("singular",try({covmat <- MSE * solve(JTJ)},TRUE))
    if(length(ErrorSolve)>0) {
      if(ErrorSolve==1) {
        if(ErrorOccured=="no"){
          cat("\n \n")
          if (!file.exists(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.txt"))){
          message("SINGULAR (ILL-CONDITIONED) JACOBIAN MATRIX FOUND.
  MODEL ESTIMATION SUMMARY METRICS ARE NOT OUTPUT. 
  RUN EXECUTION TERMINATED.
                  
  THIS CONDITION CAN BE CAUSED BY COLLINEARITIES IN TWO OR MORE EXPLANATORY
  VARIABLES. PARAMETERS WITH SMALL, NEAR ZERO ESTIMATES MAY INDICATE THE 
  PRESENCE OF COLLINEARITIES AMONG THESE OR OTHER PARAMETERS.
                  
  USERS SHOULD ELIMINATE ONE OF THE COLLINEAR VARIABLES AND RE-ESTIMATE 
  THE MODEL. TABULAR OUTPUT OF BIVARIATE CORRELATIONS FOR THE EXPLANATORY 
  VARIABLES MAY POTENTIALLY TO IDENTIFY COLLINEAR VARIABLES. CONSIDER USE OF THE 'select_corr<-yes' 
  SETTING TO EVALUATE COLLINEARITIES.

  USERS SHOULD ALSO CHECK THE EXPLANATORY VARIABLES FOR CASES WHERE ALL OR
  VIRTUALLY ALL REACHES HAVE ZERO VALUES. 
  ")
          }else{
            message(paste0("SINGULAR (ILL-CONDITIONED) JACOBIAN MATRIX FOUND.
  MODEL ESTIMATION SUMMARY METRICS ARE NOT OUTPUT. 
                    RUN EXECUTION TERMINATED.
                    
                    THIS CONDITION CAN BE CAUSED BY COLLINEARITIES IN TWO OR MORE EXPLANATORY
                    VARIABLES. PARAMETERS WITH SMALL, NEAR ZERO ESTIMATES MAY INDICATE THE 
                    PRESENCE OF COLLINEARITIES AMONG THESE OR OTHER PARAMETERS.
                    USERS SHOULD ELIMINATE ONE OF THE COLLINEAR VARIABLES AND RE-ESTIMATE 
                    THE MODEL. TABULAR OUTPUT OF BIVARIATE CORRELATIONS FOR THE EXPLANATORY 
                    VARIABLES MAY POTENTIALLY TO IDENTIFY COLLINEAR VARIABLES.  EVALUATE COLLINEARITIES USING 
                           OUTPUT FROM THE 'select_corr<-yes' METHOD : \n \n ",path_results,"estimate/", file_sum,"_explvars_correlations.pdf \n \n",
                    path_results,"estimate/", file_sum,"_explvars_correlations.txt\n \n
                    USERS SHOULD ALSO CHECK THE EXPLANATORY VARIABLES FOR CASES WHERE ALL OR
                    VIRTUALLY ALL REACHES HAVE ZERO VALUES. " 
                    ))
            shell.exec(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.txt"))
            shell.exec(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.pdf"))
            }
          ErrorOccured<-"yes"
        }
        assign("ErrorOccured","yes",envir = .GlobalEnv)
        assign("ErrorOccured","yes",envir = parent.frame())
        Message <- "ERROR"
        JacobResults <- named.list(Message)
        estimate.metrics.list <- named.list(JacobResults)
        #        return(estimate.metrics.list)
        exit()
      }
      }
    
    #covmat <- MSE * solve(JTJ)
    SEj <- sqrt(diag(covmat))
  }

  if(if_auto_scaling == "yes") {
    SEj <- SEj / SelParmValues$pscale[SelParmValues$betaconstant==0]
  }
  
  Tj <- EstimateUnscaled / SEj
  pTj <- 2*(1-pt(abs(Tj),DF))


  #####################################################
  # Leverage statistics  (dim object)
     if(k != npar) {
       Jacob <- tjacobian
       Est <- tEstimate
     } else {
       Jacob <- sparrowEsts$jacobian                   # equivalent to the SAS IML matrix 'g'
       Est <- Estimate
     }
     Jacob_inv <- solve(t(Jacob) %*% Jacob)
     leverage <- rowSums( Jacob %*% Jacob_inv * Jacob)
  #   plot(ecdf(leverage))

  #  Derivation of Variance Inflation Factors, and X`X correlation matrix, eigenvalues and eigenvectors
  #        if any(((g[##,] / nrow(g) - g[:,] ## 2) = 0) & g[:,] <> 0) then if_center = 1 ;
  #        else if_center = 0 ;
  #        if if_center then gc = g - repeat(g[:,],nrow(g),1) ; /* Center the gradients */
  #        else gc = g ;
     cJacob <- scale(Jacob,center=TRUE,scale=TRUE)  # center by subtracting mean and scaling by SD
     cJacob2 <- Jacob[,1:length(Est)]          # no centering allowed (SAS centering function needs checking)
     cJ <- t(cJacob2) %*% cJacob2   # compute the covariances
     c2 <- diag(diag(cJ)^(-0.5),nrow=length(Est),ncol=length(Est)) %*% cJ %*% diag(diag(cJ)^(-0.5),nrow=length(Est),ncol=length(Est))  # compute the correlation matrix

  # Initialize the VIF and eigenvalue vectors and corr and eigenvector matrices 
     tvif <- diag( solve(c2) )
     corr <- rbind( t(tvif), c2)    # vertical concatenation of two matrices
     Eigen <- eigen(c2)
     Eigen <- eigensort(Eigen,batch_mode,ErrorOccured)  # sort values (and corresponding vectors) in descending order
     te_val <- Eigen$values
     te_vec <- Eigen$vectors
     e_val_spread <- max(Eigen$values) / min(Eigen$values)

  # reset matrices to missing parameters
    e_vec <- array(0,dim=c(npar,npar))
    e_val <- numeric(npar)
    vif <- numeric(npar)
    ii <- 0
    for (i in 1:npar) {
#      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i]) & abs(Estimate[i]) > 1.0e-04) {
      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i])) {
       ii <- ii+1
       vif[i] <- tvif[ii]
       e_val[i] <- te_val[ii]
       jj <- 0
       for (j in 1:npar) {
#         if ((Estimate[j] != sparrowEsts$betamn[j] & Estimate[j] != sparrowEsts$betamx[j]) & abs(Estimate[j]) > 1.0e-04) {   # code correction: replaced index 'j' with 'i' for min and max (7-21-2014)
         if ((Estimate[j] != sparrowEsts$betamn[j] & Estimate[j] != sparrowEsts$betamx[j])) { 
           jj <- jj+1
           e_vec[i,j] <- te_vec[ii,jj]
         }
       }
      }
    }
    e_vec <- rbind(t(e_val),e_vec)


  # Create errors for bootstrapping. The weighted_resid includes the square root of the observation weight and the bootstrap
  # occurance weight. h_lev includes the number of occurrences in the bootstrap sample. Therefore, to remove the occurrence
  # factor it is necessary to substitute the maximum of the boot_weight or 1 in place of the 1 in the denominator term. This
  # effectively removes the boot_weight from weighted_resid. 
    weights <- Csites.weights.list$weight
    standardResids <- Resids * sqrt(weights / (RMSE^2 * (1 - leverage)))
    boot_resid <- Resids / sqrt(1 - leverage)
    boot_weights <- rep(1,length(leverage))
    mean_exp_weighted_error <- (t(boot_weights) %*% exp(boot_resid)) / sum(boot_weights) 
    
    CooksD <- ((Resids/(1-leverage))^2 * leverage) / (npar * RMSE^2)   # Helsel and Hirsch (2002)
    CooksDpvalue <- 1-pf(CooksD,(npar+1),DF)   # Cook's D p value
    
    leverageCrit <- (3*npar)/mobs   # high leverage critical value 

  # Residual normality metrics and test
   x1 <- sort(Resids)
   x2 <- sort(rnorm(n=length(leverage),mean=0,sd=1))
   ppcc <- cor(x1,x2,method="pearson") 
   shap <- shapiro.test(Resids)
   shap.p <- shap$p.value
   shap.test <- unname(shap$statistic)

  #####################################################
  # transfer estimated parameters into complete parameter vector

  parmtotal <- length(srcvar) + length(dlvvar) + length(decvar) + length(resvar) + length(othervar)
  oParmnames <- noquote(c(srcvar,dlvvar,decvar,resvar,othervar))
  
  ndeliv <- length(dlvvar)
  nsrc <- length(srcvar)

  Parmnames <- character(parmtotal)
  oEstimate <- numeric(parmtotal)
  odesign <- matrix(0,nrow=nsrc,ncol=ndeliv)
  Beta.inital <- numeric(parmtotal)
  bmin <- numeric(parmtotal)
  bmax <- numeric(parmtotal)
  bscale<- numeric(parmtotal)
  oSEj <- numeric(parmtotal)
  oTj <- numeric(parmtotal)
  opTj <- numeric(parmtotal)
  oVIF <- numeric(parmtotal)
  esttype <- character(parmtotal)
  btype <- character(parmtotal)
  oNames <- character(length(beta0))

  # Transfer parameter values
  j <- 0
  kparm <- 0
  for (i in 1:bcols) {
    if(betaconstant[i] == 0) { 
      j <- j+1
      kparm <- kparm+1
      Parmnames[j] <- oParmnames[i]
      oEstimate[j] <- round(EstimateUnscaled[kparm],digits=5)    # round coefficients to 1.e-05
      if(i <= nsrc & ndeliv > 0) { odesign[j,] <- dlvdsgn[i,] }
      Beta.inital[j] <- beta0[i]
      bmin[j] <- sparrowEsts$betamn[kparm]
      bmax[j] <- sparrowEsts$betamx[kparm]
      oSEj[j] <- round(SEj[kparm],digits=4)
      oTj[j] <- round(Tj[kparm],digits=3)
      opTj[j] <- round(pTj[kparm],digits=5)
      oVIF[j] <- round(vif[kparm],digits=5)
      if(EstimateUnscaled[kparm] != 0) {
        esttype[j] <- "Estimated"
      } else {
        esttype[j] <- "Fixed"  # condition where parameter is zero (equals min or max boundary)
      }
      btype[j] <- betatype[i]
      bscale[j] <- bsrcscale[i]
      oNames[kparm] <- oParmnames[i]    # names for output with parameter correlations and Eigenvectors

    } else {                     # non-estimated (constant) parameters
      j <- j+1
      Parmnames[j] <- oParmnames[i]
      oEstimate[j] <- beta0[i]  # SelParmValues object
      if(i <= nsrc & ndeliv > 0) {odesign[j,] <- dlvdsgn[i,]}
      Beta.inital[j] <- beta0[i]
      bmin[j] <- betamin[i]  # SelParmValues object
      bmax[j] <- betamax[i]  # SelParmValues object
      oSEj[j] <- 0
      oTj[j] <- NA
      opTj[j] <- NA
      oVIF[j] <- NA
      esttype[j] <- "Fixed"
      btype[j] <- betatype[i]    # SelParmValues object
      bscale[j] <- bsrcscale[i]  # SelParmValues object
    }
  }

}  # end if_estimate check

  ##############################
  # Obtain CLASS region numbers
    mrbgrp <- table(class[,1])   # get labels
    xx <- as.data.frame(mrbgrp)  # convert table to dataframe...
    mrbgrp <- as.numeric(levels(xx$Var1)[xx$Var1])  # convert factor levels to numeric values

  # Obtain parameter count for each CLASS
  
#   mrb_varct <- numeric(max(mrbgrp))
#   for (i in 1:length(Parmnames)) {
#    if (esttype[i] != "Fixed") {
#     name1 <- Parmnames[i]        # paste("subdata$",Parmnames[i],sep="")
#     check <- ifelse(eval(parse(text=name1)) > 0,1,0)

#     mrb_ck <- check * classrch    # revised 9-11-2014
#     mrb_table <- table(mrb_ck)
#     xy <- as.data.frame(mrb_table)
#     mrb_var <- as.numeric(levels(xy$mrb_ck)[xy$mrb_ck])  # convert factor levels to numeric values

#     for (j in 1:length(mrb_var)) {
#      if (mrb_var[j] > 0) {
#        mrb_varct[mrb_var[j]] <- mrb_varct[mrb_var[j]] + 1  # count number parameters in each class region
#      }
#     }
#    }
#   }

  # Compute SSE and RMSE for MRB regions
    SSEMRB <- numeric(length(mrbgrp))
    RMSEMRB <- numeric(length(mrbgrp))
    RMSEnn <-  numeric(length(mrbgrp))
    ii <- 0
    for (k in 1:length(xssemrb)) {
      if(xssemrb[k] != 0) {
        ii <- ii+1
        SSEMRB[ii] <- xssemrb[k]
    # RMSEMRB[ii] <- sqrt(xssemrb[k] / (xx$Freq[ii]-mrb_varct[k]))
        RMSEnn[ii] <- xx$Freq[ii]
      }
    }

####################################################
  HesResults <- alist(HesResults=)$SelParmValues$beta0   # set to NULL

  # load the Hessian results if object exists
  objfile <- paste(path_results,"/estimate/",file_sum,"_HessianResults",sep="")
  if(file.exists(objfile) == TRUE) {
    load(objfile)
  }

if(ifHess == 'yes' & if_estimate_simulation == 'no'){
  # Obtain SEs for coefficients using Hessian 

  message("Computing Hessian standard errors for parameter estimates...")
  
  ptm <- proc.time()
  
   Hssobj<-function(Estimate){                  # changed beta0 to Estimate 9-6-16
    crossprod(estimateFeval(Estimate,batch_mode,ErrorOccured))          # changed beta0 to Estimate 9-6-16
   }
   HH<-hessian(Hssobj, Estimate)    # n x n matrix
   
   ErrorSolve <- grep("singular",try({Hinv<-solve(HH)},TRUE))
   if(length(ErrorSolve)>0) {
     if(ErrorSolve==1) {
       if(ErrorOccured=="no"){
         cat("\n \n")
         if (!file.exists(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.txt"))){
           message("SINGULAR (ILL-CONDITIONED) HESSIAN MATRIX FOUND.
                   MODEL ESTIMATION SUMMARY METRICS ARE NOT OUTPUT. 
                   RUN EXECUTION TERMINATED.
                   
                   THIS CONDITION CAN BE CAUSED BY COLLINEARITIES IN TWO OR MORE EXPLANATORY
                   VARIABLES. PARAMETERS WITH SMALL, NEAR ZERO ESTIMATES MAY INDICATE THE 
                   PRESENCE OF COLLINEARITIES AMONG THESE OR OTHER PARAMETERS.
                   
                   USERS SHOULD ELIMINATE ONE OF THE COLLINEAR VARIABLES AND RE-ESTIMATE 
                   THE MODEL. TABULAR OUTPUT OF BIVARIATE CORRELATIONS FOR THE EXPLANATORY 
                   VARIABLES MAY POTENTIALLY TO IDENTIFY COLLINEAR VARIABLES. CONSIDER USE OF THE 'select_corr<-yes' 
                   SETTING TO EVALUATE COLLINEARITIES.
  
                   USERS SHOULD ALSO CHECK THE EXPLANATORY VARIABLES FOR CASES WHERE ALL OR
                   VIRTUALLY ALL REACHES HAVE ZERO VALUES. 
                   ")
         }else{
           message(paste0("SINGULAR (ILL-CONDITIONED) HESSIAN MATRIX FOUND.
                          MODEL ESTIMATION SUMMARY METRICS ARE NOT OUTPUT. 
                          RUN EXECUTION TERMINATED.
                          
                          THIS CONDITION CAN BE CAUSED BY COLLINEARITIES IN TWO OR MORE EXPLANATORY
                          VARIABLES. PARAMETERS WITH SMALL, NEAR ZERO ESTIMATES MAY INDICATE THE 
                          PRESENCE OF COLLINEARITIES AMONG THESE OR OTHER PARAMETERS.
                          USERS SHOULD ELIMINATE ONE OF THE COLLINEAR VARIABLES AND RE-ESTIMATE 
                          THE MODEL. TABULAR OUTPUT OF BIVARIATE CORRELATIONS FOR THE EXPLANATORY 
                          VARIABLES MAY POTENTIALLY TO IDENTIFY COLLINEAR VARIABLES.  EVALUATE COLLINEARITIES USING 
                          OUTPUT FROM THE 'select_corr<-yes' METHOD : \n \n ",path_results,"estimate/", file_sum,"_explvars_correlations.pdf \n \n",
                          path_results,"estimate/", file_sum,"_explvars_correlations.txt\n \n
                          USERS SHOULD ALSO CHECK THE EXPLANATORY VARIABLES FOR CASES WHERE ALL OR
                          VIRTUALLY ALL REACHES HAVE ZERO VALUES. " 
           ))
           shell.exec(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.txt"))
           shell.exec(paste0(path_results,"/estimate/", file_sum,"_explvars_correlations.pdf"))
         }
         ErrorOccured<-"yes"
       }
       assign("ErrorOccured","yes",envir = .GlobalEnv)
       assign("ErrorOccured","yes",envir = parent.frame())
       Message <- "ERROR"
       JacobResults <- named.list(Message)
       estimate.metrics.list <- named.list(JacobResults)
       #       return(estimate.metrics.list)
       exit()
     }
     }
   
   # Hinv<-solve(HH)
   
  
  HesRunTime <- proc.time() - ptm
  
  cov2<-Hinv*MSE*2                # multiplier of 2 added for parameter covariances
  cvar <- diag(cov2)
  cvar <- ifelse(cvar < 0,1,cvar) # negative variance cases coded as non-significant
  SEh<-sqrt(cvar)
  
  Hesnames <- Parmnames[SelParmValues$betaconstant==0]
    
  if(if_auto_scaling == "yes") {
    SEh <- SEh / SelParmValues$pscale[SelParmValues$betaconstant==0]
  }
  
  Th<-EstimateUnscaled/SEh
  pTh <- 2*(1-pt(abs(Th),DF))

  # parameter correlations
    cor2 <- matrix(1,nrow=length(SEh),ncol=length(SEh))
    if(length(SEh)>1) {
     for (i in 2:length(SEh)) {
       for (j in 1:i-1) {
         cor2[i,j] <- cov2[i,j] / (sqrt(cvar[i]) * sqrt(cvar[j]))
         cor2[j,i] <- cor2[i,j]
       }
     }
    }


  # transfer estimated values into completed parameter set
  oSEh <- numeric(parmtotal)
  oTh <- numeric(parmtotal)
  opTh <- numeric(parmtotal)

  # estimated parameters
  j <- 0
  kparm <- 0
  for (i in 1:bcols) {
    if(betaconstant[i] == 0) { 
      j <- j+1
      kparm <- kparm+1
      oSEh[j] <- round(SEh[kparm],digits=4)
      oTh[j] <- round(Th[kparm],digits=3)
      opTh[j] <- round(pTh[kparm],digits=5)
    } else {       # Non-estimated (constant) parameters
      j <- j+1
      oSEh[j] <- 0
      oTh[j] <- NA
      opTh[j] <- NA
    }
  }

  message("   Seconds elapsed in Hessian execution... ",round(HesRunTime[[3]],digits=5))
  
  HesResults <- named.list(Parmnames,Hesnames,oEstimate,oSEh,oTh,opTh,cov2,cor2,HesRunTime)

  # store Hessian estimates in object as list
    objfile <- paste(path_results,"/estimate/",file_sum,"_HessianResults",sep="")
    save(HesResults,file=objfile)

}   # end ifHess check

##########################################################
##########################################################
# SUMMARY PERFORMANCE METRICS FOR NO MONITORING ADJUSTMENT

  pResids <- estimateFevalNoadj(EstimateUnscaled,batch_mode,ErrorOccured)

  # Obtain summary metrics
   ppredict <- numeric(numsites)
   pyldobs <- numeric(numsites)
   pyldpredict <- numeric(numsites)
   pratio.obs.pred <- numeric(numsites)
   pxssemrb <- numeric(max(class[,1]))

   ins <- 0
   for (k in 1:nreach) {
    if (data[k,jdepvar] > 0) {
       ins <- ins+1
       obs <- data[k,jdepvar]
       Obs[ins] <- data[k,jdepvar]
       ppredict[ins] <- (exp(log(obs) - pResids[ins]))  
       pyldobs[ins] <- Obs[ins] / data[k,jtotarea] * yieldFactor
       pyldpredict[ins] <- ppredict[ins] / data[k,jtotarea] * yieldFactor
       pratio.obs.pred[ins] <- Obs[ins] / (ppredict[ins])

   # compute SSE by MRB
       if(class[ins] > 0) {
        pxssemrb[class[ins,1]] <- pxssemrb[class[ins,1]] + pResids[ins]^2
       }
    }                                 
   }  #  reach counter 
   pxssemrb[is.na(pxssemrb)] <- 0
   
   

   pSSE <- sum(pResids**2)
   pMSE <- pSSE / DF
   pRMSE <- sqrt(pMSE)

   pRSQ <- 1 - pSSE / (sum(log(Obs)^2) - sum(log(Obs))^2/mobs)
   pRSQ_ADJ <- 1 - ((mobs - 1)/DF)*(1 - pRSQ) 
   pRSQ_YLD <- 1 - pSSE / (sum(log(pyldobs)^2) - sum(log(pyldobs))^2/mobs)

   NSn <- 0
   NSd <- 0
   Obsmean <- mean(log(Obs))
   PBiasn <- 0
   PBiasd <- 0
   for (i in 1:mobs) {
     NSn <- NSn + (log(Obs[i]) - log(ppredict[i]))^2
     NSd <- NSd + (log(Obs[i]) - Obsmean)^2
     PBiasn <- PBiasn + (Obs[i] - ppredict[i])
     PBiasd <- PBiasd + Obs[i]
   }
   pNSeff <- 1 - (NSn / NSd)    # overall Nash-Sutcliffe model efficiency
   pPBias <- PBiasn / PBiasd * 100   # Percent bias  

  # Compute SSE and RMSE for MRB regions
  pSSEMRB <- numeric(length(mrbgrp))
  pRMSEMRB <- numeric(length(mrbgrp))
  ii <- 0
  for (k in 1:length(pxssemrb)) {
    if(pxssemrb[k] != 0) {
      ii <- ii+1
      pSSEMRB[ii] <- pxssemrb[k]
  # pRMSEMRB[ii] <- sqrt(pxssemrb[k] / (xx$Freq[ii]-mrb_varct[k]))
    }
  }

  ######################################################################################
  if (if_sparrowEsts == 1) {
    
   JacobResults <- named.list(Parmnames,Beta.inital,bmin,bmax,esttype,btype,bscale,
                             oEstimate,oSEj,oTj,opTj,oVIF,odesign,oNames,e_val_spread,ppcc,
                             shap.test,shap.p,mean_exp_weighted_error,boot_resid,e_vec,leverage,
                             if_auto_scaling)
   
  } else {
    Parmnames <- noquote(c(srcvar,dlvvar,decvar,resvar))
    oEstimate <- SelParmValues$beta0                              # starting values 
    Beta.inital <- SelParmValues$beta0   # starting values
    esttype <- rep("Fixed",length(SelParmValues$beta0))
    btype <- SelParmValues$betatype
    bscale <- SelParmValues$bsrcscale
    bmin <- SelParmValues$betamin
    bmax <- SelParmValues$betamax
    odesign <- dlvdsgn
    leverage <- rep(1,mobs)
    boot_resid <- rep(1,mobs)
    standardResids <- rep(1,mobs)
    CooksD <- rep(1,mobs)
    CooksDpvalue <- rep(1,mobs)
    leverageCrit <- rep(1,mobs)
    JacobResults <- named.list(Parmnames,Beta.inital,bmin,bmax,esttype,btype,bscale,
                               oEstimate,odesign,leverage,boot_resid,if_auto_scaling)
  }
  
  ANOVA.list <- named.list(mobs,npar,DF,SSE,MSE,RMSE,RSQ,RSQ_ADJ,RSQ_YLD,NSeff,PBias,
                           pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pNSeff,pPBias)
  Mdiagnostics.list <- named.list(Obs,predict,yldobs,yldpredict,xssemrb,
                                  xstaid,tarea,ratio.obs.pred,xlat,xlon,
                                  ppredict,pyldobs,pyldpredict,pratio.obs.pred,pxssemrb,
                                  mrbgrp,RMSEMRB,RMSEnn,SSEMRB,pRMSEMRB,pSSEMRB,SSEMRB,
                                  Resids,pResids,standardResids,CooksD,CooksDpvalue,leverageCrit)
    
  # store Jacobian estimates in object as list
  objfile <- paste(path_results,"/estimate/",file_sum,"_JacobResults",sep="")
  save(JacobResults,file=objfile)
  

  
  estimate.metrics.list <- named.list(JacobResults,HesResults,ANOVA.list,Mdiagnostics.list)
   

 
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("estimateNLLSmetrics.R",batch_mode)
      }
    }else{#if no error
   return(estimate.metrics.list)    
    }#end if error
    
  }#test if previous error
}#end function
