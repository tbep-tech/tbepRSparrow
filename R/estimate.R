# estimate.R

# Executes "if_estimate" yes and no options

##########################################################

 estimate <- function(if_estimate,if_predict,ConcFactor,path_results,file_sum,
                      classvar,classrchvar,dlvdsgn,
                      estimate.input.list,reach_decay_specification,reservoir_decay_specification,
                      minimum_reaches_separating_sites,
                      DataMatrixEstimate.list,DataMatrix.list,SelParmValues,Csites.weights.list,sitedata,
                      if_validate,Vsites.list,vsitedata,subdata,
                      Cor.ExplanVars.list,
                      class_landuse,sitedata.landuse,vsitedata.landuse,sitedata.demtarea.class,vsitedata.demtarea.class,
                      mapping.input.list,betavalues,
                      batch_mode,ErrorOccured,if_estimate_simulation,class_landuse_percent,
                      csv_decimalSeparator, csv_columnSeparator) {

  # INPUT objects:
  # estimate.input.list - ifHess, s_offset, ifadjust
  # if_estimate
  # if_predict
  # ConcFactor
  # ifHess
  # reach_decay_specification
  # reservoir_decay_specification
  # DataMatrixEstimate.list
  # DataMatrix.list
  # SelParmValues
  # Csites.weights.list
  # dlvdsgn
  # sitedata
  # subdata
   
  # OUTPUT objects (estimate.list):
  # sparrowEsts
  # JacobResults
  # HesResults
  # ANOVA.list
  # Mdiagnostics.list

  #########################################
   
   # create global variable from list names
   for(i in 1:length(estimate.input.list)){
     tempobj=estimate.input.list[[i]]
     eval(parse(text=paste(names(estimate.input.list)[[i]],"= tempobj")))
   }
   
   estimate.list <- NULL
   
   if (ErrorOccured=="no"){
     tryIt<-try({
 
 if (if_estimate == "yes" & if_estimate_simulation == "no") {
  message("Running estimation...")
   sparrowEsts <- estimateOptimize(path_results,file_sum,SelParmValues,estimate.input.list,DataMatrixEstimate.list,batch_mode,ErrorOccured)
  
#   Resids <- sparrowEsts$resid / sqrt(Csites.weights.list$weight)  # re-express residuals in original units
  
  # Calculate estimation diagnostic metrics (JacobResults, HesResults)
  if_sparrowEsts <- 1  # calculation of Jacobian diagnostics
  estimate.metrics.list <- estimateNLLSmetrics(if_estimate,if_estimate_simulation,ifHess,if_auto_scaling,if_sparrowEsts,sparrowEsts,
                                               path_results,file_sum,classvar,classrchvar,dlvdsgn,yieldFactor,
                                               Csites.weights.list,
                                               Csites.list,SelParmValues,subdata,sitedata,DataMatrix.list,batch_mode,ErrorOccured)

   # unpack estimate.metrics.list for return
  JacobResults <- estimate.metrics.list$JacobResults
  HesResults <- estimate.metrics.list$HesResults
  ANOVA.list <- estimate.metrics.list$ANOVA.list
  Mdiagnostics.list <- estimate.metrics.list$Mdiagnostics.list
  estimate.list <- named.list(sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list)

  
  if(if_validate == "yes") {
    message("Running Validation...")
    validate.metrics.list <- validateMetrics(classvar,classrchvar,estimate.list,dlvdsgn,Vsites.list,yieldFactor,
                                             SelParmValues,subdata,vsitedata,DataMatrix.list,batch_mode,ErrorOccured)
    vANOVA.list <- validate.metrics.list$vANOVA.list
    vMdiagnostics.list <- validate.metrics.list$vMdiagnostics.list
    estimate.list <- named.list(sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list,
                                vANOVA.list,vMdiagnostics.list)
  }
  
  
  # Output summary metrics 
  estimateNLLStable(path_results,file_sum,if_estimate,if_estimate_simulation,ifHess,if_sparrowEsts,
                    classvar,classrchvar,sitedata,
                    ANOVA.list,JacobResults,HesResults,sparrowEsts,Mdiagnostics.list,
                    Cor.ExplanVars.list,
                    if_auto_scaling,if_validate,vANOVA.list,vMdiagnostics.list,betavalues,
                    csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)
  

  ####################################################
  ### 1. Output diagnostic graphics (plots & maps) ###
  message("Running diagnostic plots and sensitivity analysis...")
  if(is.finite(JacobResults$mean_exp_weighted_error)){   # mean error and standardized residuals must be available
    diagnosticPlotsNLLS(path_results,file_sum,classvar,class_landuse,sitedata.demtarea.class,
                      sitedata,sitedata.landuse,estimate.list,mapping.input.list,Csites.weights.list,data_names,add_vars,batch_mode,ErrorOccured)
  }
  ##############################################
  ### 3. Sensitivity analyses for parameters ###
  
  diagnosticSensitivity(path_results,file_sum,classvar,estimate.list,DataMatrix.list,SelParmValues,
                        reach_decay_specification,reservoir_decay_specification,subdata,sitedata.demtarea.class,batch_mode,ErrorOccured)
  
  #####################################
  ### 4. Output validation metrics  ###

  if(if_validate == "yes") {
    diagnosticPlotsValidate(path_results,file_sum,classvar,class_landuse,vsitedata.demtarea.class,
                            vsitedata,vsitedata.landuse,estimate.list,mapping.input.list,add_vars,batch_mode,ErrorOccured)

  } # end validate loop
  ##########################################
} else { 
  
  # No estimation desired; load previous results if exist, including Jacobian to support prediction
  #   Files checked include:  sparrowEsts, HessianResults, JacobResults
  #   These files are needed for prediction and bootstrap prediction
  
  objfile <- paste(path_results,"/estimate/",file_sum,"_sparrowEsts",sep="")
  if(file.exists(objfile) == TRUE & if_estimate_simulation=="no") {
    load(objfile)
    # Load SPARROW object file from a prior run if available
    # Contents of "sparrowEsts" object:  
    # $resid - station model residuals
    # $jacobian - jacobian results
    # $coefficients - estimated NLLS values, scaled per user request
    # $coefficientsUnscaled - unscaled values in original units
    # $ssquares - total sum of square of error for the NLLS 
    # $lower - lower parameter bounds for the least squares estimation
    # $upper - upper parameter bounds for the least squares estimation
    
#    Resids <- sparrowEsts$resid / sqrt(Csites.weights.list$weight)  # re-express residuals in original units
    
    objfile <- paste(path_results,"/estimate/",file_sum,"_JacobResults",sep="")
    if(file.exists(objfile) == TRUE) {
      load(objfile)
    }
    
    estimate.list <- named.list(sparrowEsts,JacobResults)
    assign("estimate.list",estimate.list, envir=.GlobalEnv)
    
    # load the Hessian results if object exists
    objfile <- paste(path_results,"/estimate/",file_sum,"_HessianResults",sep="")
    if(file.exists(objfile) == TRUE) {
      load(objfile)
      estimate.list <- named.list(sparrowEsts,JacobResults,HesResults)
      assign("estimate.list",estimate.list, envir=.GlobalEnv)
    }
    
  ##########################################
  } else {       # no prior estimates, thus use starting values to make predictions
                 #  and create 'sparrowEsts' and 'JacobResults' objects for return
    
   if (if_estimate_simulation == "yes") {                  # ADDED 4-12-2017
message("Running model in simulation mode using initial values of the parameters...")
    sparrowEsts<-alist(SelParmValues=)$beta0
    sparrowEsts$coefficient <- SelParmValues$beta0   # starting values
    sparrowEsts$if_auto_scaling <- "no"
    nn <- ifelse(DataMatrix.list$data[,10] > 0,1,0)  # jdepvar site load index

    # if monitoring loads exist (but not estimating coefs), run residuals, performance measures, 
    #     and save JacobResults objects 
    
    if(sum(nn) > 0) {     
      
      message("Outputing performance diagnostics for simulation mode...")
      
      # compute Resids
      sparrowEsts$resid <- estimateFevalNoadj(SelParmValues$beta0,batch_mode,ErrorOccured)  # estimate using starting values
  
      sparrowEsts$coefficientsUnscaled <- SelParmValues$beta0
        
    # OUTPUT ESTIMATION METRICS & SUMMARY TABLE
      if_sparrowEsts <- 0   # no calculation of Jacobian diagnostics
      estimate.metrics.list <- estimateNLLSmetrics(if_estimate,if_estimate_simulation,ifHess,if_auto_scaling,if_sparrowEsts,sparrowEsts,
                                                   path_results,file_sum,classvar,classrchvar,dlvdsgn,yieldFactor,
                                                   Csites.weights.list,
                                                   Csites.list,SelParmValues,subdata,sitedata,DataMatrix.list,batch_mode,ErrorOccured)

      # unpack estimate.metrics.list for return
      JacobResults <- estimate.metrics.list$JacobResults
      HesResults <- estimate.metrics.list$HesResults
      ANOVA.list <- estimate.metrics.list$ANOVA.list
      Mdiagnostics.list <- estimate.metrics.list$Mdiagnostics.list
      estimate.list <- named.list(sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list)
      
      # save sparrowEsts file to support prediction
      objfile <- paste(path_results,"/estimate/",file_sum,"_sparrowEsts",sep="")
      save(sparrowEsts,file=objfile)
      
      if(if_validate == "yes" & if_estimate_simulation == "no") {
        message("Running Validation...")
        validate.metrics.list <- validateMetrics(classvar,classrchvar,estimate.list,dlvdsgn,Vsites.list,
                                                 SelParmValues,subdata,vsitedata,DataMatrix.list,batch_mode,ErrorOccured)
        vANOVA.list <- validate.metrics.list$vANOVA.list
        vMdiagnostics.list <- validate.metrics.list$vMdiagnostics.list
        estimate.list <- named.list(sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list,
                                    vANOVA.list,vMdiagnostics.list)
      }
      
      estimateNLLStable(path_results,file_sum,if_estimate,if_estimate_simulation,ifHess,if_sparrowEsts,
                        classvar,classrchvar,sitedata,
                        ANOVA.list,JacobResults,HesResults,sparrowEsts,Mdiagnostics.list,
                        Cor.ExplanVars.list,
                        if_auto_scaling,if_validate,vANOVA.list,vMdiagnostics.list,betavalues,
                        csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)
      
      
      ### Output diagnostic graphics (plots & maps) ###
      message("Running diagnostic plots and sensitivity analysis...")
      diagnosticPlotsNLLS(path_results,file_sum,classvar,class_landuse,sitedata.demtarea.class,
                          sitedata,sitedata.landuse,estimate.list,mapping.input.list,Csites.weights.list,data_names,add_vars,batch_mode,ErrorOccured)

      ### Sensitivity analyses for parameters ###
      diagnosticSensitivity(path_results,file_sum,classvar,estimate.list,DataMatrix.list,SelParmValues,
                            reach_decay_specification,reservoir_decay_specification,subdata,sitedata.demtarea.class,batch_mode,ErrorOccured)

      ### Output validation metrics  ###
      if(if_validate == "yes" & if_estimate_simulation == "no") {
        diagnosticPlotsValidate(path_results,file_sum,classvar,class_landuse,vsitedata.demtarea.class,
                                vsitedata,vsitedata.landuse,estimate.list,mapping.input.list,add_vars,batch_mode,ErrorOccured)
      } # end validate loop
      
    } else {
    
    # if no monitoring loads, store Jacobian estimates in object as list for use in making predictions only
      JacobResults<-alist(JacobResults=)$SelParmValues$beta0   # starting values 
      JacobResults$oEstimate <- SelParmValues$beta0
      JacobResults$Parmnames <- noquote(c(SelParmValues$srcvar,SelParmValues$dlvvar,
                                          SelParmValues$decvar,SelParmValues$resvar))
      JacobResults$if_auto_scaling <- "no"
      objfile <- paste(path_results,"/estimate/",file_sum,"_JacobResults",sep="")
      save(JacobResults,file=objfile)
      
      objfile <- paste(path_results,"/estimate/",file_sum,"_sparrowEsts",sep="")
      save(sparrowEsts,file=objfile)
      
      estimate.list <- named.list(sparrowEsts,JacobResults)
    }

    
   } # end if_estimate_simulation check

  } # end else for 'no prior estimates exist' case
  
 }  # end "if_estimate" check
   
   
 ##########################################
 # Compute summary statistics for predictions
   
   if(if_predict == "yes"  & if_estimate == "yes") {

     # Calculate and output standard bias-corrected predictions
     #  Note:  these include adjusted and nonadjusted for monitoring loads
     if(is.null(estimate.list$JacobResults$mean_exp_weighted_error) == TRUE) {
       bootcorrection <- 1.0
     } else {
       bootcorrection <- estimate.list$JacobResults$mean_exp_weighted_error
     }
     
     if(is.finite(bootcorrection)) {
       message("Running summary predictions...")
       predict.list <- predict(estimate.list,ConcFactor,yieldFactor,ConcUnits,yieldUnits,bootcorrection,DataMatrix.list,SelParmValues,
                               reach_decay_specification,reservoir_decay_specification,subdata,batch_mode,ErrorOccured)
       
       predictSummaryOutCSV(path_results,file_sum,ConcFactor,yieldFactor,ConcUnits,yieldUnits,
                            SelParmValues,estimate.list,predict.list,
                            subdata,class_landuse, class_landuse_percent,
                            csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)
     } else {
       message("Summary predictions not executed; mean_exp_weighted_error and predictions = infinity; 
               check standardized residuals for outliers...")
     }

   }
  
 ##########################################
       if (if_estimate=="yes"|if_estimate_simulation=="yes"){      
   for (l in names(estimate.list)){
     assign(l,get(l),envir=.GlobalEnv)   
     if (l=="Mdiagnostics.list"){
     # store Mdiagnostics.list in object as list
   objfile <- paste(path_results,"/estimate/",file_sum,"_Mdiagnostics.list",sep="")
   save(list=l,file=objfile)
   }
   }
   assign("estimate.list",estimate.list, envir=.GlobalEnv)


       }


     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("estimate.R",batch_mode)
       }
     }else{#if no error
#          if (if_estimate=="yes"|if_estimate_simulation=="yes"){return(estimate.list)}
           return(estimate.list)
     }#end if error
     
   }#test if previous error
 }#end function  # return

