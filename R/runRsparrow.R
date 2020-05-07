#' @import plyr
#' @import ggplot2
#' @import nlmrt
#' @import numDeriv
#' @import OpenMx
#' @import stringr 
#' @import gear
#' @import gplots 
#' @import rgdal
#' @import shiny
#' @import sp
#' @import spdep 
#' @import data.table
#' @import rstudioapi
#' @import svDialogs

#' @useDynLib deliv_fraction
#' @useDynLib huc_incr
#' @useDynLib mptnoder 
#' @useDynLib ptnoder 
#' @useDynLib sites_incr 
#' @useDynLib sum_atts
#' @useDynLib tnoder

if (exists("runRsparrow")){
  if (runRsparrow=="yes" & if_install_packages=="no"){    
    ErrorOccured<-"no" 
    
    #copy old model if requested
    if (!is.na(copy_PriorModelFiles)){
      copyPriorModelFiles(activeFile,copy_PriorModelFiles,path_master, ErrorOccured)
      runOld<-"yes"
    }else{
      runOld<-"no"
    }
    
    #if copying old model for run
   # if (!exists("runOld")){
  #    runOld<-"no"
  #  }
    testDir<- paste(path_user,"/",results_directoryName,"/",run_id,"/",sep="") 
    
    findControlFiles(path_user,if_userModifyData,
                     create_initial_dataDictionary, create_initial_parameterControlFiles)

    
    if (runOld=="no"){
      
      #open control files for edit
    if (edit_Parameters=="yes"){openParameters(path_user,results_directoryName)}
    if (edit_DesignMatrix=="yes"){openDesign(path_user,results_directoryName)}
    if (edit_dataDictionary=="yes"){openVarnames(path_user,results_directoryName)}

      
#Questions for user 
{removeObjects(c("saved","runScript","run2","runOld",
                 "data1","GeoLines","lineShape","polyShape","data1_priorImport",
                 "subdata","BootBetaest","predict.list","BootUncertainties",
                 "sparrowEsts","DataMatrix.list","DataMatrixEstimate.list","HesResults","JacobResults"))
  if (activeFile==""){
    message("Please select current control.R file.  Browser window may appear behind Rstudio.")
    activeFile<-file.choose()
    assign("path_user",dirname(dirname(activeFile)),envir = .GlobalEnv)
  }
  saved<-isScriptSaved(activeFile,testDir)
  if (saved!=1){
    cat("Please save active control file :\n",activeFile,"\nRun Execution Terminated.")
  }
  if (saved==1){
    #set path_main
    path_main<-path_master
    #set default values for any missing required mapping settings
    setMapDefaults(outputERSImaps,
                             
                             #prediction map settings
                             predictionTitleSize,
                             predictionLegendSize,
                             predictionLegendBackground,
                             predictionMapColors,
                             predictionClassRounding,
                             predictionMapBackground,
                             lineWidth,
                             
                             #residual maps settings
                             residual_map_breakpoints,
                             ratio_map_breakpoints,
                             residualTitleSize,
                             residualLegendSize,
                             residualColors,
                             residualPointStyle,
                             residualPointSize_breakpoints,
                             residualPointSize_factor,
                             residualMapBackground,
                             
                             #siteAttribute maps settings
                             siteAttrTitleSize,
                             siteAttrLegendSize,
                             siteAttrColors,
                             siteAttrClassRounding,
                             siteAttr_mapPointStyle,
                             siteAttr_mapPointSize,
                             siteAttrMapBackground,
                             
                             #scenarios
                             scenarioMapColors,
                   #diagnostics
                   diagnosticPlotPointSize,
                   diagnosticPlotPointStyle)
    ##test for invalid settings
    source(paste(path_main,"/R/testSettings.R",sep=""))
    if (nrow(badSettings)!=0 & ErrorOccured=="no"){
      runScript<-"no"
      cat("\n \n")
      print(badSettings)
      cat("\n \n")
      cat("Please fix all invalid settings.\nRun Execution Terminated.")
      cat("\n \n")
    }else if (ErrorOccured=="no"){
      #make global paths
      makePaths(path_user,path_master,run_id,results_directoryName,data_directoryName,gis_directoryName)
      if (ErrorOccured=="no"){
      #rename control files
      #runScript<-reNamef(activeFile,run_id,create_initial_dataDictionary,create_initial_parameterControlFiles,
      #                   if_userModifyData,batch_mode,ErrorOccured)        
runScript<-"yes"
        #generate input lists
        source(paste(path_main,"/R/generateInputLists.R",sep=""))
      
        #create initial varnames
        if (create_initial_dataDictionary=="yes"){
          createInitialDataDictionary(path_data,input_data_fileName,file_sum,path_results,
                                create_initial_parameterControlFiles,csv_decimalSeparator, csv_columnSeparator,
                                batch_mode,ErrorOccured)
          ErrorOccured<-"yes"
        }
        #create initial design matrix and betas files
        if (create_initial_parameterControlFiles=="yes"){
          createInitialParameterControls(file_sum,path_results,csv_decimalSeparator, csv_columnSeparator,
                                         batch_mode,ErrorOccured)
          ErrorOccured<-"yes"
        }
      }#if erorr occured = no
      if (ErrorOccured=="no"){
      if (runScript=="yes"){

        #test for sparrowNames found in parameters.csv but NOT in dataDictionary.csv and/or design_matrix.csv
        #terminate if missing found
        addVars(path_user,results_directoryName,csv_decimalSeparator,csv_columnSeparator, batch_mode,ErrorOccured)
        
        #create binary maps
        if (if_create_binary_maps=="yes"){
          setupMaps(path_gis,mapping.input.list,batch_mode,ErrorOccured)
        }
        #create output directories
        dirCreated<-createDirs(activeFile,run_id,path_results,batch_mode,if_userModifyData,ErrorOccured)
        
        #delete old files if_estimate or if_estimate_simulation
        if (if_estimate=="yes" | if_estimate_simulation=="yes"){
         deleteFiles(path_results, ErrorOccured)
        }
        
        ##############################################################
        if (batch_mode=="no" & ErrorOccured=="no"){    
          {cat("\n \n")
            #run2<-ifelse(run_dataImport=="yes" & load_previousDataImport=="no",1,0)
            run2<-ifelse(load_previousDataImport=="no",1,0)
            cat("RSPARROW MODEL NAME: ",file_sum,sep="")
            cat("\n \n")
            if (if_predict_scenarios=="yes" & ErrorOccured=="no"){
              cat("SCENARIO NAME: ",scenario_name,sep="")
              cat("\n \n")
            }
            cat("OUTPUT DIRECTORY: ",path_results,sep="")
            cat("\n \n")
            if (run2==1){
              dataInputPrep(#for readData
                path_data,input_data_fileName,csv_decimalSeparator, csv_columnSeparator,
                #for readVarnames
                path_results,file_sum,
                #for checkData1NavigationVars
                if_reverse_hydseq,
                #for createVerifyNavigationVars
                if_verify_demtarea,calculate_reach_attribute_list,
                path_gis,mapping.input.list,
                #for all
                batch_mode,ErrorOccured)
              
            }#if run2=yes
            if (load_previousDataImport=="yes"){
              fileName<-strsplit(path_results,"/")[[1]]
                   fileName<-paste(fileName[1:length(fileName)-1],collapse = "/")
                   fileName<-paste(fileName,"/",gsub(".csv","",input_data_fileName),"_priorImport",sep="")
                   #check if file exists
                   if (file.exists(fileName)){
                   load(file=fileName)  
                   }else{
                     ErrorOccured<-"yes"
                     message(paste("ERROR : ",fileName," binary file NOT FOUND\n SET load_previousDataImport<-'no'.\n RUN EXECUTION TERMINATED.",sep=""))
                   }
                   
              
            }
          }#wait for run2 selection
          ###############################################################
          #runRsparrow
          source(paste(path_main,"/R/startModelRun.R",sep=""))
        }else if(ErrorOccured=="no"){#batch run
          cat("\n \n")
          run2<-1
              save(list = c(as.character(outputSettings(path_results,file_sum,csv_decimalSeparator,csv_columnSeparator,FALSE,batch_mode,ErrorOccured)$setting),
                            "ErrorOccured","runScript","run2",ls()[which(regexpr("path_",ls())>0)],
                            ls()[which(regexpr("file_",ls())>0)],"estimate.input.list","mapping.input.list"),
                   file=paste(path_main,"/batch/batch.RData",sep=""))
            system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,"/batch/batchRun.R",sep="")),sep=""), wait = FALSE, invisible = FALSE)
            cat("Running RSPARROW in batch mode.")
        }
      }#if runScript="yes"
      }#if error occured=no
  }#if no invalid settings
}#if saved==1
  }#wait for saved selection 
#    }#if parameter files open, need to be closed
    }#runOld
      }#runNOw = yes
    
}#exists runNow
