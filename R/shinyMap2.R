#'@title shinyMap2
#'@description Modular Shiny app that allows the user to interactively generate Stream, Catchment, and Site Attribute maps, 
#'as well as execute Source Reduction Scenarios
#'Uses libraries shiny, sp, data.table, maptools, rgdal, shinyWidgets, stringr, and rhandsontable
#'Uses subroutines: setup routines : createInteractiveChoices, createInteractiveScenarioChoices, createRTables,
#'                  UIs : streamCatch, shinySiteAttr, shinyScenarios
#'                  MODS : compileALL, selectAll, updateVariable, shinyScenariosMod, goShinyPlot
#'@param path_results path to results directory
#'@param file_sum user specified run_id 
#'@param path_gis path to users gis data
#'@param map_uncertainties Vector of user selected uncertainty parameters to map, if uncertainty analysis was not run NA
#'@param BootUncertainties Uncertainty values if available, if uncertainty analysis was not run NA
#'@param data_names input data from data_Dictionary.csv file
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit,lon_limit,master_map_list,
#'lineShapeName,lineWaterid,polyShapeName,ployWaterid,LineShapeGeo, LineShapeGeo,CRStext,convertShapeToBinary.list,
#'map_siteAttributes.list,residual_map_breakpoints,site_mapPointScale, if_verify_demtarea_maps
#'@param subdata input data (subdata) 
#'@param SelParmValues selected parameters from parameters.csv using condition `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`

#'@return outshinyInput the Shiny Input list with hottables as dataframes and cosmetic mapping settings as list objects


shinyMap2<-function(
  #stream/catchment
  path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  #predict.list,
  subdata,SelParmValues,
  #site attr
  sitedata,
  #scenarios
  scenario_name,estimate.list,
  ConcFactor,DataMatrix.list,
  reach_decay_specification,reservoir_decay_specification,
  #scenarios out
  add_vars,csv_decimalSeparator,csv_columnSeparator,
  #batchError
  batch_mode,ErrorOccured){
  
  # if (ErrorOccured=="no"){
  #    tryIt<-try({ 
  library(shiny)
  library(sp)
  library(data.table)
  library(maptools)
  library(rgdal)
  library(shinyWidgets)
  library(stringr)
  library(rhandsontable)
  library(leaflet)
  library(sf)
  library(mapview)
  library(magrittr)
  
  
  #load predicitons if available
  if (file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))){
    load(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))
  }
  
  #estimation objects
  if (file.exists(paste(path_results,"/estimate/",file_sum,"_JacobResults",sep=""))){
    if (!exists("JacobResults")){
      load(paste(path_results,"/estimate/",file_sum,"_JacobResults",sep=""))
    }
  }
  

  

  
  #set up variable choices
  choices<-createInteractiveChoices(SelParmValues,exists("predict.list"),subdata, data_names, map_uncertainties)
  scenarioChoices<-createInteractiveScenarioChoices()
  
  #map type choices
  if (exists("predict.list") & exists("JacobResults")){
    mapTypeChoices<-c("","Stream","Catchment","Site Attributes","Source Reduction Scenarios")
    selectSources<-as.character(JacobResults$Parmnames[which(JacobResults$btype=="SOURCE")]) 
  #  scenarioRtables<-createRTables(selectSources,data_names,mapping.input.list)

  }else{
    mapTypeChoices<-c("","Stream","Catchment","Site Attributes")
    selectSources<-""
  }
  
  scenarioRtables<-createRTables(selectSources,data_names,mapping.input.list)
  
  #setup shiny ui
  shinyApp(  ui=shinyUI(
    
    fluidPage(tags$head(
                       tags$style("h5{color: red}")),
      titlePanel(
                 h1(paste("Rshiny Interactive Map : ",file_sum,sep=""),h5(div(HTML("DO NOT CLICK ON ITEMS ABOVE THIS POINT!"))))),
      
      sidebarLayout(
        sidebarPanel(width=6,
                     #tags$head(
                    #   tags$style("h5{color: black}")),
                     h4("SPARROW Interactive Mapping                     "),
                     br(),
                     
                     #top level user input
                     selectInput("batch","Output Mode",c("Interactive","Batch")),
                     selectInput("mapFormat", "Map Format", c('Dynamic', 'Static')),
          
                     selectInput("mapType","Map Type",mapTypeChoices),
                    
                     #Stream and Catchment arguments
                     streamCatch("nsStreamCatch", input, choices, map_uncertainties),
                     
                     #site Attribute arguments
                     shinySiteAttr("nsSiteAttr",input,choices),
                     
                     #scenarios arguments
                     shinyScenarios("nsScenarios",input),
                     
                     #output shape file ifBatch
                     shapeFunc("nsBatch",input),
                     
                    # actionButton("showInput","Show Input"),
                     conditionalPanel(
                       condition = "input.batch=='Interactive'",
                       fluidRow(
                         actionButton("goPlot","Generate Plot"),
                         actionButton("savePDF", "SaveAs PDF"))       
                       
                     ),
                     
                     conditionalPanel(
                       condition = "input.batch=='Batch'",
                       actionButton("batchPlot","Save Plot(s)")      
                     )
                     ),

        mainPanel(width = 6,
          conditionalPanel(
            condition = "input.mapFormat=='Static'",
            plotOutput("plotOne", width=900,height=900)
          ),
          conditionalPanel(
            condition = "input.mapFormat=='Dynamic'",
            leafletOutput("plotTwo", height = 800)
          )
        )

        ))) #end ui function
    ,

    ################################################################
    ###############################################################
    ###############################################################
    
    server=shinyServer(function(input, output,session) {

      #compile all user input
     # observeEvent(input$showInput,{
        
      #  compileALL<-compileALL(input, output, session, path_results, choices)
      #  compiledInput<-compileALL$compiledInput


        
       # output$txtOut<-renderPrint({compiledInput})
    #  })
      
     #select all and clear all buttons in drop downs 
     observe({        
       if (input$batch=="Batch" & input$mapType %in% c("Stream","Catchment","Site Attributes")){      
         
         lapply(1:length(as.character(unique(choices$category))), function(c) {
           category<-as.character(unique(choices$category))[c]
           if (category!="Prediction Uncertainties"){
             nsName<-paste0("ns",tolower(str_split(category," ")[[1]][1]),"Drop")
           }else{
             nsName<-"nsuncertaintyDrop"
           }
             callModule(selectAll,nsName, category = category, choices = choices)
           })
         callModule(selectAll,"nsattrDrop", category = "Data Dictionary Variable", choices = choices)

         }
     })
     
     #update variable lists according to variable type selection in interactive mode
     observe({
     if (input$batch=="Interactive" & input$mapType %in% c("Stream","Catchment")){     
         callModule(updateVariable,"nsStreamCatch", choices= choices, mapType = input$mapType)

     }else if (input$batch=="Interactive" & input$mapType == "Site Attributes"){
       callModule(updateVariable,"nsSiteAttr", choices= choices, mapType = input$mapType)
     }
       })
     

     #rTables 
     observe({
       if (input$mapType %in% c("Source Reduction Scenarios")){
        callModule(shinyScenariosMod,"nsScenarios",scenarioRtables)
        #callModule(handsOnMod, "nsScenarios-nsAllSources", DF = scenarioRtables$allSourcesDF )
         }else if (input$mapType %in% c("Stream","Catchment")){
           callModule(handsOnMod, "nsStreamCatch-nsCosmetic", DF = as.data.frame(scenarioRtables$cosmeticPred))
             }else if (input$mapType == "Site Attributes"){
                callModule(handsOnMod, "nsSiteAttr-nsCosmetic", DF = as.data.frame(scenarioRtables$cosmeticSite))
         }
     })

     #interactive plot
     p <- eventReactive(input$goPlot, {
       #run plot

       goShinyPlot(input, output, session, choices,"goPlot",
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
                   batch_mode,ErrorOccured)
       
     })
     
     observe({
     
       if(input$mapFormat == 'Static')
         output$plotOne <- renderPlot(p())

       if(input$mapFormat == 'Dynamic')
         output$plotTwo <- renderLeaflet(p())
       
     })
    
     #pdf output
     observeEvent(input$savePDF, {
       goShinyPlot(input, output, session, choices,"savePDF",
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
                   batch_mode,ErrorOccured)
       })#end pdf output
     
     #batchplot
     observeEvent(input$batchPlot, {
       goShinyPlot(input, output, session, choices,"batchPlot",
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
                   batch_mode,ErrorOccured)
       })#end batch plot
     session$onSessionEnded(function() {
       stopApp()
     }) 
     })#end server function
)#end shinyApp function
}#end ShinyMap2    
  
#  shiny::runApp(shinyMap2(#stream/catchment
#    path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
#    data_names,mapping.input.list,
#    #predict.list,
#    subdata,SelParmValues,
#    #site attr
#    sitedata,
#    #scenarios
#    scenario_name,estimate.list,
#    ConcFactor,DataMatrix.list,
#    reach_decay_specification,reservoir_decay_specification,
#    #scenarios out
#    add_vars,csv_decimalSeparator,csv_columnSeparator,
#    #batchError
#    batch_mode,ErrorOccured))               
                     
                     