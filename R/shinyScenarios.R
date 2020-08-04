shinyScenarios<-function(id, input, scenario_name){
  ns<-NS(id)

  conditionalPanel(
    condition = "input.mapType == 'Source Reduction Scenarios'",
    
    ##output map type
    conditionalPanel(
      condition = "input.batch=='Batch'",
      checkboxGroupInput(ns("outCheck"), "Select Output Map Type", 
                         c("Stream","Catchment"),
                         inline=TRUE)),
    conditionalPanel(
      condition = "input.batch!='Batch'",
      selectInput(ns("outType"),"Select output map type",c("Stream","Catchment"))),
   
    #scenario_name 
    fluidRow(
      column(width=9,textInput(ns("scenarioName"), "Scenario Name", scenario_name)),
      column(width=3,checkboxGroupInput(ns("overwriteScenario"), "","Overwrite",
                                        inline=TRUE))
    ),
    
    #if_predict_scenarios
    selectInput(ns("domain"), "Spatial Domain", c("all reaches","selected reaches")),
    
    conditionalPanel(
      condition = paste0("input['",ns("domain"),"'] == 'selected reaches'"),
      selectInput(ns("allSrc"),"Apply same reach selection criteria to all selected sources (yes/no)",selected = "",c("","yes","no")),
      
    
      conditionalPanel(
      condition = paste0("input['",ns("allSrc"),"']=='yes'"),
      h4("Select Sources and Percent Reduction Factors"),
      h6("Right click on Row to insert above/below or remove row"),
      handsOnUI(ns("nsSourceRed"),input),
      h4("Reach Selection Criteria"),
      h5("(Reach Selection Criteria applys to all sources)"),
      
      handsOnUI(ns("nsAllSources"),input)
     
       )
      ,#end conditional apply to all sources
      conditionalPanel(
        condition = paste0("input['",ns("allSrc"),"']=='no'"),
        h4("Reach Selection Criteria"),
        h6("Right click on Row to insert above/below or remove row"),
        handsOnUI(ns("nsAllSourcesNO"),input)
        
      )#end conditional apply to all sourcesNO
    ),#end conditional selected reaches
    conditionalPanel(
      condition = paste0("input['",ns("domain"),"'] == 'all reaches'"),
      h4("Select Sources and Percent Reduction Factors"),
      h6("Right click on Row to insert above/below or remove row"),
      handsOnUI(ns("nsSourceRedALL"),input)
      ),
   
     h4("Mapping Settings"),
    handsOnUI(ns("nsCosmetic"),input)
    
    )#end wrap all conditional
}