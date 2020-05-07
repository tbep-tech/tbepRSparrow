#
# estimateNLLStable.R
#
####################################################
# OUTPUT TABULAR RESULTS
#

estimateNLLStable <- function(path_results,file_sum,if_estimate,if_estimate_simulation,ifHess,if_sparrowEsts,
                              classvar,classrchvar,sitedata,
                              ANOVA.list,JacobResults,HesResults,sparrowEsts,Mdiagnostics.list,
                              Cor.ExplanVars.list,
                              if_auto_scaling,if_validate,vANOVA.list,vMdiagnostics.list,betavalues,
                              csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured) {
  
  # INPUT objects:
  # ANOVA.list
  # JacobResults
  # HesResults
  # sparrowEsts
  # Mdiagnostics.list
  # sitedata
  # Csites.weights.list
  
  # Printed text output
  if (ErrorOccured=="no"){
    tryIt<-try({

 filename <- paste(path_results,"/estimate/",file_sum,"_summary.txt",sep="")
 dir.create(paste(path_results,"/estimate/summaryCSV",sep=""))
fileCSV<-paste(path_results,"/estimate/summaryCSV/",sep="")
 sink(file=filename,split="FALSE",append=FALSE)

  # create global variable from list names (ANOVA.list)
  for(i in 1:length(ANOVA.list)){
    tempobj=ANOVA.list[[i]]
    eval(parse(text=paste(names(ANOVA.list)[[i]],"= tempobj")))
  }

  # create global variable from list names (JacobResults)
  for(i in 1:length(JacobResults)){
    tempobj=JacobResults[[i]]
    eval(parse(text=paste(names(JacobResults)[[i]],"= tempobj")))
  }

  # create global variable from list names (HesResults)
  if(ifHess == "yes" & if_estimate_simulation == 'no') {
     for(i in 1:length(HesResults)){
       tempobj=HesResults[[i]]
       eval(parse(text=paste(names(HesResults)[[i]],"= tempobj")))
     }
  }

  # create global variable from list names (Mdiagnostics.list)
  for(i in 1:length(Mdiagnostics.list)){
   tempobj=Mdiagnostics.list[[i]]
   eval(parse(text=paste(names(Mdiagnostics.list)[[i]],"= tempobj")))
  }
 
  # create global variable from list names (Csites.weights.list)
  for(i in 1:length(Csites.weights.list)){
   tempobj=Csites.weights.list[[i]]
   eval(parse(text=paste(names(Csites.weights.list)[[i]],"= tempobj")))
  }
 
 # create global variable from list names (Cor.ExplanVars.list)
 if(!is.na(Cor.ExplanVars.list)) {
    for(i in 1:length(Cor.ExplanVars.list)){
      tempobj=Cor.ExplanVars.list[[i]]
      eval(parse(text=paste(names(Cor.ExplanVars.list)[[i]],"= tempobj")))
    }
 }


 #get description and units from betavalues
 betavalues<-betavalues[,which(names(betavalues) %in% c("sparrowNames","description","parmUnits"))]
 
  # define title output function
  outcharfun<-function(char) {
    outchar <- data.frame(char)
    row.names(outchar ) <- c(" ")
    colnames(outchar ) <- c(" ")
    return(outchar)
  }

  # define "space" for printing
  ch <- character(1)
  space <- data.frame(ch)
  row.names(space) <- ch
  colnames(space) <- c(" ")


  ##################################
  options(width = 500, max.print=50000) 

  print(outcharfun("SPARROW NLLS MODEL SUMMARY"))

  print(outcharfun(paste("MODEL NAME: ",file_sum,sep="")))

  print(outcharfun(paste("FILE PATH: ",filename,sep="")))

  print(space)
  dd <- data.frame(mobs,npar,DF,SSE,MSE,RMSE,RSQ,RSQ_ADJ,RSQ_YLD,PBias)
  colnames(dd) <- c("MOBS","NPARM","DF","SSE","MSE","RMSE","RSQ","RSQ-ADJUST","RSQ-YIELD","PERCENT BIAS")
  ch <- character(1)
  row.names(dd) <- ch
  
  # only output estimation performance is model estimated (i.e., simulation)
  if (if_estimate == 'yes') {
  print(outcharfun("MODEL ESTIMATION PERFORMANCE (Monitoring-Adjusted Predictions)"))
  print(dd)
  fileout<-paste(fileCSV,"ModelPerformanceMonitoringAdj.csv",sep="")
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  }
  
  dd <- data.frame(mobs,npar,DF,pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pPBias)
  colnames(dd) <- c("MOBS","NPARM","DF","SSE","MSE","RMSE","RSQ","RSQ-ADJUST","RSQ-YIELD","PERCENT BIAS")
  ch <- character(1)
  row.names(dd) <- ch
  print(outcharfun("MODEL SIMULATION PERFORMANCE (Simulated Predictions)"))
  print(dd)
  fileout<-paste(fileCSV,"ModelPerformanceNoMonitoringAdj.csv",sep="")
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  if (if_estimate == 'yes') {
     writeLines("\n   Simulated predictions are computed using mean coefficients from the NLLS model \n     that was estimated with monitoring-adjusted (conditioned) predictions\n")
  }
  
  if(if_validate == "yes" & if_estimate_simulation == "no"){
    
    # create global variable from list names (vANOVA.list)
    for(i in 1:length(vANOVA.list)){
      tempobj=vANOVA.list[[i]]
      eval(parse(text=paste(names(vANOVA.list)[[i]],"= tempobj")))
    }
    dd <- data.frame(mobs,npar,DF,pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pPBias)
    colnames(dd) <- c("MOBS","NPARM","DF","SSE","MSE","RMSE","RSQ","RSQ-ADJUST","RSQ-YIELD","PERCENT BIAS")
    ch <- character(1)
    row.names(dd) <- ch
    print(outcharfun("MODEL VALIDATION PERFORMANCE (Simulated Predictions)"))
    print(dd)
    fileout<-paste(fileCSV,"ModelValidationNoMontinoringAdj.csv",sep="")
    fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  }

  print(outcharfun("PARAMETER SUMMARY"))
  # print parameter estimates w/o standard errors
  if(JacobResults$if_auto_scaling == "no" & if_estimate == "yes") {
    bscale <- 1.0  # set to no scaling if not requested; over-rides manual scaling that is automatically applied in 'selectParmValues.R'
  }
  dd <- data.frame(Parmnames,oEstimate,btype,esttype,Beta.inital,bmin,bmax,bscale)
  colnames(dd) <- c("PARAMETER","ESTIMATE","PARM TYPE","EST TYPE","INITIAL VALUE","MIN","MAX","SCALE")
  dd$rname <- as.numeric(row.names(dd))
  dd<-merge(dd,betavalues, by.y ="sparrowNames",by.x="PARAMETER")
  dd <- dd[with(dd,order(dd$rname)), ]
  dd <- within(dd, rm(rname))  # drop rname
  colnames(dd)<-c("PARAMETER","ESTIMATE","PARM TYPE","EST TYPE","INITIAL VALUE","MIN","MAX","SCALE","DESCRIPTION","PARAMETER UNITS")
  ch <- character(1)                 # changed from (2)  9-11-2014
  for (i in 1:length(Parmnames)) {
    ch[i] <- as.character(i)
  }
  row.names(dd) <- ch
  print(dd,right=FALSE)
  fileout<-paste(fileCSV,"ParameterSummary.csv",sep="")
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
 if(sum(ifelse(esttype=="Fixed",1,0))>0) {     # at least one Fixed parameter type found
   writeLines("\n   A 'Fixed' parameter estimation type (EST TYPE) indicates a user choice of a constant \n     coefficient value or a coefficient estimate equal to zero, the minimum or maximum  \n     boundary value (this may indicate a statistically insignificant parameter or a \n     parameter with a likely value outside of the current bounds).")
 }
  
  if(NLLS_weights=="lnload") {     # option executed weighted SPARROW optimization
    writeLines("\n   The model was estimated with a weighted error variance. The weights are proportional \n     to the log predicted load to account for heteroscedasticity.")
  }
  
 if(JacobResults$if_auto_scaling == "yes" & if_estimate == "yes") {
   print(outcharfun(" Parameter scaling applied for model estimation"))
 } else {
   if(if_estimate == "yes") {
     print(outcharfun(" Parameter scaling not applied for model estimation"))
   } else {
     if(if_estimate_simulation == "yes") {
       print(outcharfun(" Model simulation executed using intial values of parameters"))
     }
   }
 }

 if (if_estimate == "yes" & if_estimate_simulation == 'no') {
  
   # print parameter estimates with Jacobian SEs
   ddJ <- data.frame(Parmnames,btype,oEstimate,oSEj,oTj,opTj,oVIF)
   ddJ$rname <- as.numeric(row.names(ddJ))
   colnames(ddJ) <- c("PARAMETER","PARM TYPE","ESTIMATE","SE(Jcb)","T(Jcb)","P-VALUE(Jcb)","VIF","rname")
   ddJ<-merge(ddJ,betavalues, by.y ="sparrowNames",by.x="PARAMETER")
   colnames(ddJ) <- c("PARAMETER","PARM TYPE","ESTIMATE","SE(Jcb)","T(Jcb)","P-VALUE(Jcb)","VIF","rname","DESCRIPTION","PARAMETER UNITS")
   ch <- character(1)
   for (i in 1:length(Parmnames)) {
    ch[i] <- as.character(i)
   }
   row.names(ddJ) <- ch
   if(ifHess == 'no'){
    print(outcharfun("PARAMETER ESTIMATES"))
    ddJ <- ddJ[with(ddJ,order(ddJ$rname)), ]
    ddJ <- within(ddJ, rm(rname))  # drop rname
    ch <- character(1)
    for (i in 1:length(Parmnames)) {
      ch[i] <- as.character(i)
    }
    row.names(ddJ) <- ch
    print(ddJ,right=FALSE)
    fileout<-paste(fileCSV,"ParameterEstimates.csv",sep="")
    fwrite(ddJ,file=fileout,row.names=F,append=F,quote=T,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    print(space)
   }
  

   if(ifHess == 'yes' & if_estimate_simulation == 'no'){
    # print parameter estimates with Hessian SEs
    ddH <- data.frame(Parmnames,btype,oEstimate,oSEh,oTh,opTh)
    ddH$rname <- as.numeric(row.names(ddH))
    colnames(ddH) <- c("PARAMETER","PARM TYPE","ESTIMATE","SE","T","P-VALUE","rname")
    dd<-merge(ddH,ddJ, by=c("rname","PARAMETER","PARM TYPE","ESTIMATE"))
    dd <- dd[with(dd,order(dd$rname)), ]
    dd <- within(dd, rm(rname,"SE(Jcb)","T(Jcb)","P-VALUE(Jcb)"))  # drop rname and Jacobian metrics
    ch <- character(1)
    for (i in 1:length(Parmnames)) {
      ch[i] <- as.character(i)
    }
    row.names(dd) <- ch
    print(outcharfun("PARAMETER ESTIMATES"))
    print(dd,right=FALSE)
    fileout<-paste(fileCSV,"ParameterEstimates.csv",sep="")
    fwrite(dd,file=fileout,row.names=F,append=F,quote=T,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
   }
    print(space)
      dd <- data.frame(e_val_spread,ppcc,shap.test,shap.p,mean_exp_weighted_error)
    colnames(dd) <- c("EigenValue Spread"," Normal PPCC"," SWilks W"," P-Value","  Mean Exp Weighted Error")
    ch <- " "
    row.names(dd) <- ch
    print(dd)
    fileout<-paste(fileCSV,"EigenValueSpread.csv",sep="")
    fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")

    # print design matrix selections for model execution
    if(sum(ifelse(JacobResults$btype=="DELIVF",1,0)) > 0) {
      dd <- as.data.frame(odesign)
      ndeliv <- ncol(odesign)
      nsrc <- nrow(odesign)
      row.names(dd) <- Parmnames[1:nsrc]
      colnames(dd) <- Parmnames[nsrc+1:ndeliv]
      print(outcharfun("DESIGN MATRIX"))
      print(dd)
      fileout<-paste(fileCSV,"DesignMatrix.csv",sep="")
      fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
             dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    }
  
 } # end if_estimate check


 # Residuals
 print(space)
 print("LOG RESIDUALS, Station quantiles",quote=FALSE)
 print(quantile(round(sparrowEsts$resid,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
 fileout<-paste(fileCSV,"LogResid_StationQuantiles.csv",sep="")
 dd<-as.data.frame(t(quantile(round(sparrowEsts$resid,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
 fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
 
 # Standardized Residuals
 if(if_estimate == "yes" & if_estimate_simulation=="no"){
 x <- Mdiagnostics.list$standardResids
 if(exists("x")) {
   if(is.finite(JacobResults$mean_exp_weighted_error)){
 print(space)
 print("STANDARDIZED RESIDUALS, Station quantiles",quote=FALSE)
 print(quantile(round(Mdiagnostics.list$standardResids,digits=3),c(0.025,0.16,0.2,0.3,0.5,0.7,0.84,0.9,0.97)))
 fileout<-paste(fileCSV,"StandardResid_StationQuantiles.csv",sep="")
 dd<-as.data.frame(t(quantile(round(Mdiagnostics.list$standardResids,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
 fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
 
 if(JacobResults$mean_exp_weighted_error>1.0E+3) {
   message("
  WARNING: THE Mean Exp Weighted Error PRINTED IN THE SUMMARY TEXT FILE
  IS EXCESSIVELY LARGE. THIS IS CAUSED BY A LARGE LEVERAGE AND MODEL RESIDUAL
  FOR A STATION. CHECK THE DATA FOR THE OUTLYING STATION. ALSO CONSIDER 
  RE-ESTIMATING THE MODEL USING DIFFERENT INITIAL PARAMETER VALUES OR AFTER 
  ELIMINATING VARIABLES WITH SMALL AND STATISTICALLY INSIGNIFICANT 
  ESTIMATED COEFFICIENTS.
  ")
 }
   } else {
      message("
   WARNING: THE Mean Exp Weighted Error IS UNDEFINED, CAUSED BY A LEVERAGE VALUE OF ONE. 
   A PARAMETER MAY HAVE BEEN IMPROPERLY ESTIMATED.
   EVALUATE DIFFERENT INITIAL VALUES FOR THE PARAMETERS, INCLUDING INITIAL VALUES
   CLOSER TO THE ESTIMATED UNSCALED COEFFICIENT VALUES, OR ELIMINATE VARIABLES WITH SMALL
   AND STATISTICALLY INSIGNIFICANT ESTIMATED COEFFICIENTS.
   DIAGNOSTIC PLOTS WERE NOT OUTPUT.")
   }
 }}
 
 # Prediction accuracy statistics
 print(space)
 print("RATIO OF OBSERVED TO PREDICTED LOAD, Station quantiles",quote=FALSE)
 print(quantile(round(ratio.obs.pred,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
 fileout<-paste(fileCSV,"RatioObsToPredLoad_StationQuantiles.csv",sep="")
 dd<-as.data.frame(t(quantile(round(ratio.obs.pred,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
 fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
 
 # Observed yield statistics
 print(space)
 print("OBSERVED YIELD, percentiles",quote=FALSE)
 print(summary(yldobs))
 fileout<-paste(fileCSV,"ObservedYieldPercentiles.csv",sep="")
 dd<-as.data.frame(t(unclass(summary(yldobs))))
 fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")

 # Prediction yield statistics
 print(space)
 print("PREDICTED YIELD, percentiles",quote=FALSE)
 print(summary(yldpredict))
 fileout<-paste(fileCSV,"PredictedYieldPercentiles.csv",sep="")
 dd<-as.data.frame(t(unclass(summary(yldpredict))))
 fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")

 
 if(if_validate == "yes" & if_estimate_simulation == "no"){
   vresids <- vMdiagnostics.list$pResids
   vratio <- vMdiagnostics.list$pratio.obs.pred
 # Validation Residuals
   print(space)
   print(outcharfun("MODEL VALIDATION (simulated predictions)"))
   print("LOG RESIDUALS, Station quantiles",quote=FALSE)
   print(quantile(round(vresids,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
   fileout<-paste(fileCSV,"LogResid_StationQuantiles_NoMonitoringAdj.csv",sep="")
   dd<-as.data.frame(t(quantile(round(vresids,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
   fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
          dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
 # Validation accuracy metrics
   print(space)
   print(outcharfun("MODEL VALIDATION (simulated predictions)"))
   print("RATIO OF OBSERVED TO PREDICTED LOAD, Station quantiles",quote=FALSE)
   print(quantile(round(vratio,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
   fileout<-paste(fileCSV,"RatioObsToPredLoad_StationQuantiles_NoMonitoringAdj.csv",sep="")
   dd<-as.data.frame(t(quantile(round(vratio,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
   fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
          dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
 }
 
 # output largest outliers
 Resids <- sparrowEsts$resid
 residCheck <- abs(standardResids)
 dd <- data.frame(sitedata,standardResids,Resids,leverage,leverageCrit,CooksD,CooksDpvalue,residCheck,weight,tiareakm2)
 dd1 <- subset(dd,dd$residCheck > 3 | dd$leverage > dd$leverageCrit | dd$CooksDpvalue < 0.10)
 keeps <- c("waterid","demtarea","rchname","station_id","station_name","staid",
            classvar[1],"standardResids","Resids","leverage","leverageCrit","CooksD","CooksDpvalue","weight","tiareakm2","residCheck")
 ddnew <- dd1[keeps]
# ddnew <- dd[keeps]
# ddnew <- subset(ddnew,(ddnew$residCheck > 3 | is.na(ddnew$residCheck)))
# ddnew<- ddnew[with(ddnew,order(ddnew$residCheck)), ]
 print(space)
 print("LARGEST OUTLIERS",quote=FALSE)
 print("(absolute standardized residual>3, leverage>Critical value, or Cook's D p-value<0.10)",quote=FALSE)
 print(ddnew)
 fileout<-paste(fileCSV,"LargestSqResid.csv",sep="")
 fwrite(ddnew,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")

 # output CLASS region performance 
 print(space)
 v <- rep(1:length(RMSEMRB),1)
 dd <- data.frame(mrbgrp,RMSEnn,SSEMRB)
 colnames(dd) <- c("REGION","NUMBER OF SITES","SSE")
 ch <- character(1)
 for (i in 1:length(v)) {
   ch[i] <- as.character(i)
 }
 row.names(dd) <- ch
 print(outcharfun("REGIONAL MODEL PERFORMANCE (Monitoring-Adjusted Predictions)"))
 print(dd)
 fileout<-paste(fileCSV,"ClassRegionModelPerformance_MonitoringAdj.csv",sep="")
 fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")

# check_preds <- ifelse( pSSEMRB < SSEMRB, 1,0)

 print(space)
 v <- rep(1:length(pRMSEMRB),1)
 dd <- data.frame(mrbgrp,pSSEMRB)
 colnames(dd) <- c("REGION","SSE")
 ch <- character(1)
 for (i in 1:length(v)) {
   ch[i] <- as.character(i)
 }
 row.names(dd) <- ch
 print(outcharfun("REGIONAL MODEL PERFORMANCE (Simulated Predictions)"))
 print(dd)
 fileout<-paste(fileCSV,"ClassRegionModelPerformance_NoMonitoringAdj.csv",sep="")
 fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")


 if(ifHess == 'yes' & if_estimate_simulation == 'no'){
   # Output parameter covariances, correlations, and Eigenvalues and Eigenvectors
   # Covariances
   dd <- data.frame(cov2)
   colnames(dd) <- Hesnames
   ch <- character(1)
   ch <- Hesnames
   row.names(dd) <- ch
   
   if(JacobResults$if_auto_scaling == "yes") {
     print(outcharfun("PARAMETER COVARIANCES (scaled units)"))
   } else {
     print(outcharfun("PARAMETER COVARIANCES"))
   }
   print(dd)
   print(space)
   fileout<-paste(fileCSV,"ParameterCovariances.csv",sep="")
   fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
          dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
   # Correlations
   dd <- data.frame(cor2)
   colnames(dd) <- Hesnames
   ch <- character(1)
   ch <- Hesnames
   row.names(dd) <- ch
   print(outcharfun("PARAMETER CORRELATIONS"))
   print(dd)
   print(space)
   fileout<-paste(fileCSV,"ParameterCorrelations.csv",sep="")
   fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
          dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
   # Eigenvectors
   dd <- data.frame(e_vec)
   ch <- " "
   colnames(dd) <- rep(ch,times=ncol(dd))
   rNames <- Hesnames
   ch <- character(1)
   ch[1] <- "EigenValues"
   for (i in 2:(length(rNames)+1) ) {
      ch[i] <- rNames[i-1]
   }
   row.names(dd) <- ch
   print(outcharfun("X'X EIGENVALUES AND EIGENVECTORS"))
   print(dd)
   print(space)
   fileout<-paste(fileCSV,"EigenvaluesEigenvectors.csv",sep="")
   fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
          dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
 } # end 'ifHess'

 # Explanatory variable correlations
 if (select_corr == "yes" & !is.na(Cor.ExplanVars.list)) {
   
   if(numsites>2){
     print(outcharfun("CORRELATION MATRICES FOR EXPLANATORY VARIABLES (Site Incremental Areas)"))
     print(outcharfun("SPEARMAN CORRELATIONS FOR ALL OBSERVATIONS"))
     print(cor.allValuesM)
     
     print(outcharfun("SUMMARY METRICS FOR EXPLANATORY VARIABLES (Site Incremental Areas)"))
     print(summary(cmatrixM_all))
     
     print(outcharfun("FILTERED SUMMARY METRICS FOR EXPLANATORY VARIABLES (zero values converted to minimum of non-zero values)"))
     print(summary(cmatrixM_filter))
   }
   
   print(space)
   print(space)
   print(outcharfun("CORRELATION MATRICES FOR EXPLANATORY VARIABLES (Reaches)"))
   print(outcharfun("SPEARMAN CORRELATIONS FOR ALL OBSERVATIONS"))
   print(cor.allValues)
   xtext <- paste("SPEARMAN CORRELATIONS FOR SUBSAMPLE OF OBSERVATIONS (n=",nsamples,")",sep="")
   print(outcharfun(xtext))
   print(cor.sampleValues)
   print(outcharfun("SPEARMAN CORRELATIONS FOR SUBSAMPLED LOGGED OBSERVATIONS (zero values are converted to minimum of non-zero values)"))
   print(cor.sampleLogValues)
   
   print(outcharfun("SUMMARY METRICS FOR EXPLANATORY VARIABLES (Reaches)"))
   print(summary(cmatrix_all))
   
   print(outcharfun("FILTERED SUMMARY METRICS FOR EXPLANATORY VARIABLES (zero values converted to minimum of non-zero values)"))
   print(summary(cmatrix_filter))
 }
 
 sink(type="message")
 sink()

 # output Residuals file
 Resids <- sparrowEsts$resid
 Obsyield <- Obs / sitedata$demtarea
 predictYield <- predict / sitedata$demtarea


 origWaterid<-sitedata$waterid_for_RSPARROW_mapping
 dd <- data.frame(sitedata,origWaterid,Obs,predict,Obsyield,predictYield,Resids,standardResids,leverage,leverageCrit,CooksD,CooksDpvalue,boot_resid,weight,tiareakm2,pResids,
                  ratio.obs.pred,pratio.obs.pred,xlat,xlon)
 
 keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
            "predict","Obsyield","predictYield","Resids","standardResids","leverage","leverageCrit","CooksD","CooksDpvalue","boot_resid","weight","tiareakm2","pResids",
            "ratio.obs.pred","pratio.obs.pred","xlat","xlon")
 ddnew <- dd[keeps] 
 if (length(na.omit(add_vars))!=0){
   add_data<-data.frame(sitedata[,which(names(sitedata) %in% add_vars)])
   if (length(add_vars)==1){
     names(add_data)<-add_vars
   }
   ddnew<-cbind(ddnew,add_data)
 }
 ddnew$rchname <- str_replace_all(ddnew$rchname,"[[:punct:]]","")
 ddnew$station_name <- str_replace_all(ddnew$station_name,"[[:punct:]]","")
 fileout <- paste(path_results,"/estimate/",file_sum,"_residuals.csv",sep="")
 fwrite(ddnew,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
        dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
 
},TRUE)#end try

if (class(tryIt)=="try-error"){#if an error occured

  sink()
  if(ErrorOccured=="no"){
    errorOccurred("estimateNLLStable.R",batch_mode)
  }
}else{#if no error
   
}#end if error

}#test if previous error
}#end function
