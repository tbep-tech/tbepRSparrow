#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("batchMaps.R","batch.RData",res))
  
  
  #load subroutines
  routines<-c("checkBinaryMaps.R",
              "errorOccurred.R",
              "predictMaps.R",
              "mapBreaks.R",
              "named.list.R")
  for (r in routines){
    source(paste(path_main,"/R/",r,sep=""))
  }

  
  
  #load libraries
  library(sp)
  library(rgdal)
  library(maptools)
  
  
  #start sink
  sink(file=paste(path_results,"/maps/batch/",run_id,"_log.txt",sep=""),split=FALSE)
  cat("\n \n")
  cat("RSPARROW MODEL NAME: ",file_sum,sep="")
  cat("\n \n")
  if (if_predict_scenarios=="yes"){
    cat("SCENARIO NAME: ",scenario_name,sep="")
    cat("\n \n")
  }
  cat("OUTPUT DIRECTORY: ",path_results,sep="")
  cat("\n \n")
  
  
  #load objects
  ptm <- proc.time()
  
  #method 1
  if (file.exists(paste(path_results,"/predict/",run_id,"_predictList",sep=""))){
     load(paste(path_results,"/predict/",run_id,"_predictList",sep="")) }
  #load individual object previously saved

  load(paste(path_results,"/data/subdata",sep=""))
  # load all other required objects
  load(paste(path_results,"/maps/batch/batch.RData",sep=""))
  

  
  #run predictMaps
  #Rshiny input
  #input$batch
  #input$scenarioName
  #input$mapType
  #input$var
   if (Rshiny==FALSE){
  input<-list(mapType=NA, batch="no",scenarioName="",var="")
   }
  if (mapScenarios==FALSE){
    scenario_map_list<-NA
    predictScenarios.list<-NA
    scenario_name<-NA
  }
  
  predictMaps(#Rshiny
    input, allMetrics, output_map_type,Rshiny,
    #regular
    path_results,file_sum,path_gis,
              #map_uncertainties,BootUncertainties,
              data_names,mapping.input.list,
              #predict.list,
              subdata,
    #scenarios
    mapScenarios,
    scenario_map_list,
    predictScenarios.list,
    scenario_name,
    batch_mode,ErrorOccured)
  

  
  #end sink
  sink()
  
}

