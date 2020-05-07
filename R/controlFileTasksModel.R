#'@title controlFileTasksModel
#'@description Executes all model execution tasks
#'Uses subroutines: estimate, diagnosticSpatialAutoCorr, estimateBootstraps,
#'predict, predictOutCSV, predictBootstraps, predictBootsOutCSV,
#'predictStreamMaps, predictCatchmentMaps, predictScenarios,
#'predictScenariosOutCSV, predictStreamMapsScenarios, named.list, errorOccurred.
#'@param path_results path to results directory
#'@param file_sum user specified run_id 
#'@param SelParmValues selected parameters from parameters.csv using condition `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`
#'@param Csites.weights.list regression weights as proportional to incremental area size
#'@param subdata input data (subdata) 
#'@param DataMatrixEstimates.list named list of 'data' and 'beta' matrices and 'data.index.list' for optimization w/ scaled parameters
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' for optimization
#'@param sitedata subdata[(subdata$depvar > 0), ]
#'@param Vsites.list named list of sites for validation
#'@param vsitedata sitedata for validation
#'@param classvar character vector of user specified spatially contiguous discrete classification variables from sparrow_control
#'@param classrchvar 
#'@param class_landuse character vector of class_landuses from sparrow_control
#'@param Cor.ExplanVars.list Spearman Rho correlations among user-selected explanatory variables
#'@param data_names input data from data_Dictionary.csv file\
#'@param minimum_reaches_separating_sites number indicating the minimum number of reaches separating sites
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_predict yes/no indicating whether or not prediction is run
#'@param ConcFactor the concentration conversion factor, computed as Concentration = load / discharge * ConcFactor
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param if_validate yes/no indicating whether or not validation is run
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from sparrow_control
#'@param sitedata.landuse Land use for incremental basins for diagnostics.  Calculated by `calcIncremLandUse(subdata,class_landuse,subdata$staidseq,minimum_reaches_separating_sites,batch_mode,ErrorOccured)`
#'@param vsitedata.landuse Land use for incremental basins for diagnostics for validation sites. Calculated by `calcIncremLandUse(subdata,class_landuse,subdata$vstaidseq,minimum_reaches_separating_sites,batch_mode,ErrorOccured)`
#'@param sitedata.demtarea.class Total drainage area classification variable for calibration sites. Calculated by `calcDemtareaClass(sitedata$demtarea,batch_mode,ErrorOccured)`
#'@param vsitedata.demtarea.class Total drainage area classification variable for calibration sites in validation. Calculated by `calcDemtareaClass(vsitedata$demtarea,batch_mode,ErrorOccured)`
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit,lon_limit,master_map_list,
#'lineShapeName,lineWaterid,polyShapeName,ployWaterid,LineShapeGeo, LineShapeGeo,CRStext,convertShapeToBinary.list,
#'map_siteAttributes.list,residual_map_breakpoints,site_mapPointScale, if_verify_demtarea_maps 
#'@param iseed User specified initial seed for the bootstraps from sparrow_control
#'@param biters User specified number of parametric bootstrap iterations from sparrow_control
#'@param scenario_name User specified name of source reduction scenario from sparrow_control.  Used to create directory for source reduction scenario results.
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@param if_estimate_simulation yes/no character string indicating if simulation is to be performed using the initial parameter values
#'@param class_landuse_percent numeric vector of percentages to apply to class_landuses from sparrow_control
#'@param csv_decimalSeparator decimal separator for csv output
#'@param csv_columnSeparator column separator for csv output
#'@return named list of run times for bootstrap estimation, bootstrap prediction, Mapping, and estimate.list


# controlFileTasksModel.R

# Executes all model execution tasks

##########################################################

controlFileTasksModel <- function(
                                  # pathnames
                                  path_results,file_sum,
                                  # parameters
                                  SelParmValues,
                                  # NLR weights
                                  Csites.weights.list,
                                  # data
                                  subdata,DataMatrixEstimates.list,DataMatrix.list,sitedata,Vsites.list,vsitedata,
                                  # land use classifcation 
                                  classvar,classrchvar,class_landuse,
                                  # explanatory variable correlations
                                  Cor.ExplanVars.list,
                                  # estimation
                                  data_names,minimum_reaches_separating_sites,
                                  if_estimate,if_predict,dlvdsgn,if_validate,
                                  estimate.input.list,reach_decay_specification,reservoir_decay_specification,
                                  # diagnostics
                                  sitedata.landuse,vsitedata.landuse,sitedata.demtarea.class,vsitedata.demtarea.class,
                                  # mapping 
                                  mapping.input.list,
                                  # bootstrapping
                                  iseed,biters,
                                  #scenarios
                                  scenario_name,
                                  #batch_mode
                                  batch_mode,
                                  #error trap
                                  ErrorOccured,
                                  if_estimate_simulation,
                                  #for yield by landuse
                                  class_landuse_percent,
                                  #for read-write csv files
                                  csv_decimalSeparator, csv_columnSeparator
                                  ) {

  # INPUT objects:
   
  # OUTPUT objects:
  #  runTimes (HesRunTime,BootEstRunTime,BootPredictRunTime)

  # create global variable from list names
  for(i in 1:length(estimate.input.list)){
    tempobj=estimate.input.list[[i]]
    eval(parse(text=paste(names(estimate.input.list)[[i]],"= tempobj")))
  }
  
  #####################################
  ### PERFORM MODEL EXECUTION TASKS ###
  #####################################
  
  #######################
  ### A. OPTIMIZATION ###
  #######################

  # Estimation options for if_estimate=""
  # (1) if_estimate="yes" - executes NLLS; calculates estimation metrics, and outputs ANOVA summary table
  # (2) if_estimate="no" and prior NLLS execution completed (sparrowEsts object) - calculates estimation,
  #      metrics, and outputs ANOVA summary table (Hessian output if previously executed)
  # (3) if_estimate="no" stores starting beta values in JacobResults for use in making predictions
  #      if monitoring loads exist, then calculates estimation metrics, and outputs ANOVA summary table
  # (4) if_predict="yes" - executes reach summary deciles; output to (prefix name)_summary_predictions.csv
  
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  # creates or checks for prior estimation objects:  sparrowEsts,JacobResults,HesResults
  estimate.list <- estimate(if_estimate,if_predict,ConcFactor,path_results,file_sum,
                            classvar,classrchvar,dlvdsgn,
                            estimate.input.list,reach_decay_specification,reservoir_decay_specification,
                            minimum_reaches_separating_sites,
                            DataMatrixEstimate.list,DataMatrix.list,SelParmValues,Csites.weights.list,sitedata,
                            if_validate,Vsites.list,vsitedata,subdata,
                            Cor.ExplanVars.list,
                            class_landuse,sitedata.landuse,vsitedata.landuse,sitedata.demtarea.class,vsitedata.demtarea.class,
                            mapping.input.list,betavalues,
                            batch_mode,ErrorOccured,if_estimate_simulation,class_landuse_percent,
                            csv_decimalSeparator, csv_columnSeparator)
  

#  HesRunTime <- estimate.list$HesResults$HesRunTime
    
  ######################
  ### B. DIAGNOSTICS ###
  ######################
  
  if(if_diagnostics == "yes") {   
    
    nn <- ifelse(DataMatrix.list$data[,10] > 0,1,0)  # jdepvar site load index (monitoring sites must be present)
    if( (if_estimate=="yes" | if_estimate_simulation=="yes") & (sum(nn) > 0) ) {
      
      ############################################
      ### 2. Spatial auto-correlation analysis ###
      ############################################
      # Moran's I analysis (requires execution of diagnosticPlotsNLLS)
      message("Running diagnostic spatial autocorrelation...")
      # only execute if stream 'length' available for all reaches
      if(sum(!is.na(subdata$length))>0) {
        diagnosticSpatialAutoCorr(path_results,file_sum,classvar,sitedata,estimate.list,estimate.input.list,subdata,
                                  csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)
      }
      
    } else {
      message("Diagnostic spatial autocorrelation was not executed; 
      execution requires if_estimate or if_estimate_simulation = yes and monitoring site loads must be available.")
    }
      
  } # end if_diagnostics for production of graphics
  
  
  #####################################
  ### C. BOOTSTRAP MODEL ESTIMATION ###
  #####################################
  
  # Run parametric bootstrap option for parameter estimation (if_boot_estimate = "yes")
  # Obtain previous results if exist (if_boot_estimate = "no")
  BootEstRunTime <- " "
  ptm <- proc.time()
  
  objfile <- paste(path_results,"/estimate/",file_sum,"_HessianResults",sep="") # needed for covariance 
  
  if(if_boot_estimate == "yes" & file.exists(objfile) == TRUE) {
    message("Estimating bootstrap model coefficients and errors...")
    BootResults <- estimateBootstraps(iseed,biters,estimate.list,path_results,file_sum,
                                      csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)
    
  } else {  # no bootstrap estimation; check to see if already exists
    
    objfile <- paste(path_results,"/estimate/",file_sum,"_BootBetaest",sep="")  # BootResults
    if(file.exists(objfile) == TRUE) {
      load(objfile)
    } else {
      objfile <- paste(path_results,"/estimate/",file_sum,"_HessianResults",sep="") # needed for covariance 
      if(if_boot_estimate == "yes" & file.exists(objfile) == FALSE) {
        message(paste(" \nWARNING : HesResults DOES NOT EXIST.  boot_estimate NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
        if (batch_mode=="yes"){
          cat(paste(" \nWARNING : HesResults DOES NOT EXIST.  boot_estimate NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
        } 
      }
    }
    
  } # end check whether bootstrap file exist
  BootEstRunTime <- proc.time() - ptm
  # 250 secs for 10 iterations for bootstrap estimation
  
  
  #############################
  ### D. OUTPUT PREDICTIONS ###
  #############################
  
  # Load predictions:
  #   pload_total                Total load (fully decayed)
  #   pload_(sources)            Source load (fully decayed)
  #   mpload_total               Monitoring-adjusted total load (fully decayed)
  #   mpload_(sources)           Monitoring-adjusted source load (fully decayed)
  #   pload_nd_total             Total load delivered to streams (no stream decay)
  #   pload_nd_(sources)         Source load delivered to streams (no stream decay)
  #   pload_inc                  Total incremental load delivered to streams
  #   pload_inc_(sources)        Source incremental load delivered to streams
  #   deliv_frac                 Fraction of total load delivered to terminal reach
  #   pload_inc_deliv            Total incremental load delivered to terminal reach
  #   pload_inc_(sources)_deliv  Source incremental load delivered to terminal reach
  #   share_total_(sources)      Source shares for total load (percent)
  #   share_inc_(sources)        Source share for incremental load (percent)
  
  # Yield predictions:
  #   Concentration              Concentration based on decayed total load and discharge
  #   yield_total                Total yield (fully decayed)
  #   yield_(sources)            Source yield (fully decayed)
  #   myield_total               Monitoring-adjusted total yield (fully decayed)
  #   myield_(sources)           Monitoring-adjusted source yield (fully decayed)
  #   yield_inc                  Total incremental yield delivered to streams
  #   yield_inc_(sources)        Source incremental yield delivered to streams
  #   yield_inc_deliv            Total incremental yield delivered to terminal reach
  #   yield_inc_(sources)_deliv  Source incremental yield delivered to terminal reach
  
  # Uncertainty predictions (requires prior execution of bootstrap predictions):
  #   se_pload_total             Standard error of the total load (percent of mean)
  #   ci_pload_total             95% prediction interval of the total load (percent of mean)
  
  #############################################
  #### 1. Standard Predictions to CSV file ####
  #############################################
  gpclibPermit()
  BootPredictRunTime <- " "
  MapPredictRunTime <- " "
  
  if(if_predict == "yes") {
    
   objfile <- paste(path_results,"/estimate/",file_sum,"_JacobResults",sep="")
   if(file.exists(objfile) == TRUE) {
    
     message("Running predictions...")     

    # Calculate and output standard bias-corrected predictions
    #  Note:  these include adjusted and nonadjusted for monitoring loads
    if(is.null(estimate.list$JacobResults$mean_exp_weighted_error) == TRUE) {
      bootcorrection <- 1.0
    } else {
      bootcorrection <- estimate.list$JacobResults$mean_exp_weighted_error
    }
    
    predict.list <- predict(estimate.list,ConcFactor,yieldFactor,ConcUnits,yieldUnits,bootcorrection,DataMatrix.list,SelParmValues,
                            reach_decay_specification,reservoir_decay_specification,subdata,batch_mode,ErrorOccured)
    objfile <- paste(path_results,"/predict/",file_sum,"_predictList",sep="")
    save(predict.list,file=objfile)
    predictOutCSV(path_results,file_sum,estimate.list,predict.list,subdata,add_vars,data_names,
                  csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)
    
   }  # end check on JacobResults exist
  } # end if_predict    
    
    ###############################################
    #### 2. Bootstrap Prediction to CSV file ######
    ###############################################
    
    # Requires prior execution of bootstrap estimation and standard predictions
    ptm <- proc.time()
  if(if_boot_predict == "yes") { 
    
    if (file.exists(paste(path_results,"/estimate/",file_sum,"_BootBetaest",sep="")) &
        (file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep="")) | if_predict == "yes")  )  {
      
      load(paste(path_results,"/estimate/",file_sum,"_BootBetaest",sep=""))
      load(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))
      
      assign("BootResults",BootResults,envir = .GlobalEnv)
      
      message("Running bootstrap predictions...")
      predictBoots.list <- predictBootstraps(iseed,biters,estimate.list,predict.list,BootResults,confInterval,
                                             DataMatrix.list,SelParmValues,
                                             reach_decay_specification,reservoir_decay_specification,
                                             subdata,ConcFactor,yieldFactor,path_results,file_sum,batch_mode,ErrorOccured)

      predictBootsOutCSV(path_results,file_sum,estimate.list,predictBoots.list,subdata,add_vars,data_names,
                         csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)   # edit preditBootsOutCSV accordingly
      
    } else {
      message(paste(" \nWARNING : BootBestest DOES NOT EXIST.  boot_predict NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
       if (batch_mode=="yes"){
         cat(paste(" \nWARNING : BootBestest DOES NOT EXIST.  boot_predict NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
       }   
    }#end if file.exists
          
  }  # end _BootBestest check
    BootPredictRunTime <- proc.time() - ptm
  
  
  ########################################
  #### 3. Predictions Mapping Options ####
  ########################################
  
  ptm <- proc.time()
  if(!is.na(master_map_list[1])) {
    
  #  # obtain uncertainties, if available
  #  objfile <- paste(path_results,"/predict/",file_sum,"_BootUncertainties",sep="")
  #  if(file.exists(objfile) == TRUE) {
  #    load(objfile)
  #    map_uncertainties <- c("se_pload_total","ci_pload_total")
  #  } else {
  #    map_uncertainties <- NA 
  #    BootUncertainties <- NA
  #  }
    
    
  #  if (file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep="")) | if_predict == "yes"){
  #    if (!exists("predict.list")){
  #    load(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))
  #    }
    
      #predictMaps
      if (!is.na(output_map_type)){

    dir.create(paste(path_results,"/maps/batch",sep=""))
        mapScenarios<-FALSE
        Rshiny<-FALSE
        allMetrics<-NA
    save(list = c(as.character(outputSettings(path_results,file_sum,csv_decimalSeparator,csv_columnSeparator,FALSE,batch_mode,ErrorOccured)$setting),
                  "ErrorOccured","runScript","run2",ls()[which(regexpr("path_",ls())>0)],"path_gis",
                  ls()[which(regexpr("file_",ls())>0)],"estimate.input.list","mapping.input.list","mapScenarios","Rshiny","allMetrics"),
         file=paste(path_main,"/batch/batch.RData",sep=""))
    
    #method 1
    #save(list=c("map_uncertainties","BootUncertainties",
    #            "data_names"),
    #     file=paste(path_results,"/maps/batch/batch.RData",sep=""),compress = FALSE)
    save(list=c("data_names"),
         file=paste(path_results,"/maps/batch/batch.RData",sep=""),compress = FALSE)

    message("Running Prediction Maps in batch mode.")
    system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,"/batch/batchMaps.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
    

    unlink(paste(path_results,"/maps/batch",sep=""),recursive=TRUE)

    
      }
      
    
    #} else {
    #   message(' \nWARNING : No mapping executions because predictions are not available\n ')
    #  if (batch_mode=="yes"){
    #    cat(' \nWARNING : No mapping executions because predictions are not available\n ')
    #  }
    #      }#end if file.exists(predictList) or if_predict="yes"
    
  }
  MapPredictRunTime <- proc.time() - ptm
  
  #######################################################
  #### 4. Prediction Load-Reduction Scenario Options ####
  #######################################################
  
  # NOTE: standard predictions ("if_predict = yes") must be executed to support this feature
  #       to ensure creation of load and yield matrices
  
 # if(if_predict_scenarios != "none") {
#    options(warn=-1)
#    dir.create(paste(path_results,"/scenarios/",scenario_name,sep=""))
#    options(warn=0)
   # # Calculate and output with bias-corrected predictions
  #  if(is.null(estimate.list$JacobResults$mean_exp_weighted_error) == TRUE) {
  #    bootcorrection <- 1.0
  #  } else {
  #    bootcorrection <- estimate.list$JacobResults$mean_exp_weighted_error
  #  }
  #  if (file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep="")) | if_predict == "yes"){
  #    if (!exists("predict.list")){
   #     load(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))
  #    }
      
      # perform checks on scenario variable names designated by user
      #  scenario only executed if all source variables match
   #   vcheck<-0
  #    for (i in 1:length(estimate.list$JacobResults$Parmnames)) {
  #      for (j in 1:length(scenario_sources)) {
  #        if(scenario_sources[j] == estimate.list$JacobResults$Parmnames[i]) {vcheck<-vcheck+1}
   #     }
  #    }
  #    if(vcheck == length(scenario_sources)) {  # source names match

  #    message("Running predict scenarios...")
    #  predictScenarios.list <- predictScenarios(predict.list,
    #                                          if_predict_scenarios,scenario_sources,scenario_all_factors,
    #                                          data_names,estimate.list,ConcFactor,bootcorrection,DataMatrix.list,SelParmValues,
    #                                          reach_decay_specification,reservoir_decay_specification,subdata,batch_mode,ErrorOccured)
    #Rshiny input
    #input$variable
    #input$scLoadCheck
    #input$batch
    #input$scYieldCheck
    #input$domain
    #input$selectReaches
    #input$sourcesCheck
    #input$factors
    input<-list(variable="",scLoadCheck="",batch="",scYieldCheck="",domain="",selectReaches="",sourcesCheck="",factors="")
    predictScenarios(#Rshiny
      input,NA, output_map_type,FALSE,
      #regular
      predict.list,if_predict_scenarios,scenario_sources,scenario_all_factors,
      data_names,estimate.list,ConcFactor, if_predict,
      #bootcorrection,
      DataMatrix.list,SelParmValues,
      reach_decay_specification,reservoir_decay_specification,subdata,
      #predictStreamMapScenarios
      path_results,file_sum,path_gis,
      #scenarios out
      add_vars,csv_decimalSeparator,csv_columnSeparator,
      mapping.input.list,
      batch_mode,ErrorOccured)
     # predictScenariosOutCSV(path_results,file_sum,estimate.list,predictScenarios.list,subdata,add_vars,
    #                       scenario_name,data_names,csv_decimalSeparator, csv_columnSeparator,
    #                       batch_mode,ErrorOccured)
    
      # stream maps
     #   if(!is.na(scenario_map_list[1])) {
    #      message("Running scenario mapping...")
    #      predictStreamMapsScenarios(path_results,file_sum,path_gis,scenario_map_list,
    #                             data_names,mapping.input.list,predictScenarios.list,subdata,scenario_name,batch_mode,ErrorOccured)
    #    }
      
     # }  else { # check on variable names; names do not match
    #    message(' \nWARNING : No scenarios executed because source variables are not correct in scenario_sources setting\n ')
    #    if (batch_mode=="yes"){
    #      cat(' \nWARNING : No scenarios executed because source variables are not correct in scenario_sources setting\n ')
     #   }
    #  }#end if scenarios
      
      
    #} else {
    # message(' \nWARNING : No scenarios executed because baseline predictions are not available\n ')
    #  if (batch_mode=="yes"){
    #    cat(' \nWARNING : No scenarios executed because baseline predictions are not available\n ')
    #  }
    #}#end if file.exists(predictList) or if_predict="yes"
    
    # catchment maps (option needs to be added)
 # }
  

     
 ##########################################

   runTimes <- named.list(BootEstRunTime,BootPredictRunTime,MapPredictRunTime,estimate.list) 
  


    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("controlFileTasksModel.R",batch_mode)
      }
    }else{#if no error
      return(runTimes)
    }#end if error
    
  }#test if previous error
}#end function

