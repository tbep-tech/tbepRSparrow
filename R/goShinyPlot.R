goShinyPlot<-function(input, output, session, choices, button,
                            path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
                            data_names,mapping.input.list,
                            #predict.list,
                            subdata,SelParmValues,
                            #site attr
                            sitedata,#estimate.list,Mdiagnostics.list,
                            #scenarios
                            scenario_name,JacobResults,
                            ConcFactor,DataMatrix.list,
                            reach_decay_specification,reservoir_decay_specification,
                            #scenarios out
                            add_vars,csv_decimalSeparator,csv_columnSeparator,
                            #batchError
                            batch_mode,ErrorOccured){
 
 # p<-eventReactive(button, {
 browser()
    compileALL<-compileALL(input, output, session, path_results, choices)
    compiledInput<-compileALL$compiledInput
    compiledInput<-convertHotTables(compiledInput)
   # badSetting<-checkInteractiveSettings(compiledInput,path_results)
    
 #   if (badSetting==FALSE){
      
      if (button=="savePDF" | input$batch=="Batch"){
        if (!dir.exists(paste(path_results,"/maps/Interactive/",sep=""))){
          dir.create(paste(path_results,"/maps/Interactive/",sep=""))
        }
        if (input$mapType=="Stream"){
          if (!dir.exists(paste(path_results,"/maps/Interactive/Stream/",sep=""))){
            dir.create(paste(path_results,"/maps/Interactive/Stream/",sep=""))
          }
          filename<- paste(path_results,"/maps/Interactive/Stream/",file_sum,"_",compiledInput$var,".pdf",sep="")
          batchFilename<-paste(path_results,"/maps/Interactive/Stream/batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData",sep="")
          
        }else if (input$mapType=="Catchment"){
          if (!dir.exists(paste(path_results,"/maps/Interactive/Catchment/",sep=""))){
            dir.create(paste(path_results,"/maps/Interactive/Catchment/",sep=""))
          }
          filename<- paste(path_results,"/maps/Interactive/Catchment/",file_sum,"_",compiledInput$var,".pdf",sep="")
          batchFilename<-paste(path_results,"/maps/Interactive/Catchment/batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData",sep="")
          
        }else if (input$mapType=="Site Attributes"){
          if (!dir.exists(paste(path_results,"/maps/Interactive/SiteAttributes/",sep=""))){
            dir.create(paste(path_results,"/maps/Interactive/SiteAttributes/",sep=""))
          }
          filename<- paste(path_results,"/maps/Interactive/SiteAttributes/",file_sum,"_SiteAttributes_",compiledInput$var,".pdf",sep="")
          batchFilename<-paste(path_results,"/maps/Interactive/SiteAttributes/batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData",sep="")
          
        }else{#add check for if scenario exists ask user if proceed
          if (!dir.exists(paste(path_results,"/scenarios/",compiledInput$scenarioName,"/",sep=""))){
            dir.create(paste(path_results,"/scenarios/",compiledInput$scenarioName,"/",sep=""))
          }
          filename<- paste(path_results,"/scenarios/",compiledInput$scenarioName,"/",
                           compiledInput$scenarioName,"_",file_sum,"_",compiledInput$variable,".pdf",sep="")
          batchFilename<-paste(path_results,"/scenarios/",compiledInput$scenarioName,"/batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData",sep="")
          
        }
        
        if (input$batch!="Batch"){
        pdf(filename)
      }
      }
    
   # Modal processing message
  dataModal <- function() {
    modalDialog(
      title = "Please Wait Processing Map Request...",
      
      footer = tagList(
        modalButton("OK")
      )
    )
  }
  

    
    #set mapping input variable
    if (input$batch!="Batch"){
    if (input$mapType=="Stream" | input$mapType=="Catchment"){
      showModal(dataModal())
     # print(compiledInput)
     # compiledInput<-convertHotTables(compiledInput)
      mapScenarios<-FALSE

      if(input$mapFormat == 'Static'){
   
        out <- predictMaps(compiledInput,NA,output_map_type,TRUE,
                    path_results,file_sum,path_gis,
                    data_names,mapping.input.list,
                    subdata,
                    #scenarios
                    mapScenarios,
                    scenario_map_list,
                    predictScenarios.list,
                    scenario_name,
                    batch_mode,ErrorOccured)
      
        return(out)
      }
      
      if(input$mapFormat == 'Dynamic'){
        out <- predictMapsleaflet(compiledInput,NA,output_map_type,TRUE,
                    path_results,file_sum,path_gis,
                    data_names,mapping.input.list,
                    subdata,
                    #scenarios
                    mapScenarios,
                    scenario_map_list,
                    predictScenarios.list,
                    scenario_name,
                    batch_mode,ErrorOccured)
        return(out)
      }
      
    }else if (input$mapType=="Site Attributes"){
      showModal(dataModal())

      mapSiteAttributes(#Rshiny
        compiledInput,NA, path_gis, sitedata, LineShapeGeo,data_names,TRUE,
        #regular
        mapColumn,mapdata,GeoLines,mapping.input.list,
        strTitle,unitAttr,batch_mode,ErrorOccured)
      

      
    }else if (input$mapType=="Source Reduction Scenarios"){
      showModal(dataModal())
       #     compiledInput<-convertHotTables(compiledInput)
      compiledInput<-sourceRedFunc(compiledInput)
      
      
      unlink(list.files(paste(path_results,"/scenarios/",compiledInput$scenarioName,"/",sep=""),full.names = TRUE),recursive = TRUE)
      predictScenarios(#Rshiny
        compiledInput,NA, tolower(as.character(compiledInput$outType)),TRUE,
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

    }
 
  if (button=="savePDF"){
    dev.off()
    
    showModal(modalDialog(
      title = "",
      "Plot save action complete",
      footer = tagList(
        modalButton("OK")
      )
    ))
  }
 
        }else{#end interactive start batch
          showModal(modalDialog(
            title = "",
            "Running batch plot output.  DO NOT CLOSE Rhiny or Rstudio while batch plot output is running!",
            footer = tagList(
              modalButton("OK")
            )
          ))
         
          if (input$mapType=="Source Reduction Scenarios"){
          compiledInput<-sourceRedFunc(compiledInput)
         }
          
          inputShiny<-compiledInput
          
          if (exists("predict.list")){
            saveList<-c(      #interactiveStream
              "inputShiny","path_results","file_sum","path_gis","map_uncertainties","BootUncertainties",
              "data_names","mapping.input.list","predict.list","subdata","SelParmValues","LineShapeGeo",
              "lineShapeName","lineWaterid",
              #iinteractiveSiteAttr
              "sitedata", "LineShapeGeo",
              "estimate.list","Mdiagnostics.list",
              #interactiveScenarios
              "if_predict_scenarios","scenario_sources","scenario_all_factors","if_predict","scenario_map_list",
              "ConcFactor","DataMatrix.list",
              "reach_decay_specification","reservoir_decay_specification","dlvdsgn",
              #scenarios out
              "add_vars","csv_decimalSeparator","csv_columnSeparator",
              #all
              "batch_mode","ErrorOccured")
          }else{
            saveList<-c(      #interactiveStream
              "inputShiny","path_results","file_sum","path_gis","map_uncertainties","BootUncertainties",
              "data_names","mapping.input.list","subdata","SelParmValues","LineShapeGeo",
              "lineShapeName","lineWaterid",
              #iinteractiveSiteAttr
              "sitedata", "LineShapeGeo",
              "estimate.list","Mdiagnostics.list",
              #interactiveScenarios
              "if_predict_scenarios","scenario_sources","scenario_all_factors","if_predict","scenario_map_list",
              "ConcFactor","DataMatrix.list",
              "reach_decay_specification","reservoir_decay_specification","dlvdsgn",
              #scenarios out
              "add_vars","csv_decimalSeparator","csv_columnSeparator",
              #all
              "batch_mode","ErrorOccured")
          }
          save(list = saveList,
               
               file=batchFilename)
          
          #save batchfileName to batch folder in master
          save(list = c("path_main","batchFilename"),
               file=paste(path_main,"/batch/interactiveBatch.RData",sep=""))
          
          system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,"/batch/interactiveBatchRun.R",sep="")),sep=""), wait = FALSE, invisible = FALSE)
          
          
          
    }  #end batch
  
  #  }#check bad setting
    
 
    
 # })
  
 # return(p())
  
}