#'@title interactiveScenarioSettings
#'@description Generates list of scenario map setting checks and error messages for Rshiny in Interactive mode
#'@param input interactive user input in Shiny app
#'@return interactiveSetting a list object with test=setting tests and message = error messages

interactiveScenarioSettings<-function(input){
  interactiveSetting<-list()
  interactiveSetting$test<-c( "!is.na(input$outType)",
                              "input$outType!=''",
                             
                              "!is.na(input$scenarioName)",
                             "input$scenarioName!=''",
                             
                             "length(input$overwriteScenario)==0 
            & dir.exists(paste(path_results,'/scenarios/',input$scenarioName,'/',sep=''))",
                             
                             "length(input$sourcesCheck)==0",
                             
                             "!is.na(input$factors)",
                             "input$factors!=''",
                             "class(eval(parse(text=input$factors)))=='numeric'",
                             "lengthFactors<-length(eval(parse(text=input$factors)))
                             lengthFactors!=length(input$sourcesCheck)",
                             
                             "!is.na(input$scenarioMapColors)",
                             "input$scenarioMapColors!=''",
                             "length(eval(parse(text=input$scenarioMapColors)))>2",
                             "class(eval(parse(text=input$scenarioMapColors)))=='character'",
                             
                             "!is.na(input$scenarioClassRounding)",
                             "input$scenarioClassRounding!=''",
                             "class(input$scenarioClassRounding)=='integer'",
                             
                             "if (input$outType=='Stream'){!is.na(input$scenariolineWidth)
                              }else{TRUE}",
                             "if (input$outType=='Stream'){input$scenariolineWidth!=''
                              }else{TRUE}",
                             "if (input$outType=='Stream'){class(input$scenariolineWidth) %in% c('numeric','integer')
                             }else{TRUE}",
                            
                             "!is.na(input$scenarioTitleSize)",
                             "input$scenarioTitleSize!=''",
                             "class(input$scenarioTitleSize) %in% c('numeric','integer')",
                             
                             "!is.na(input$scenarioMapBackground)",
                             "input$scenarioMapBackground!=''",
                             "class(input$scenarioMapBackground)=='character'",
                             
                             "!is.na(input$scenarioLegendBackground)",
                             "input$scenarioLegendBackground!=''",
                             "class(input$scenarioLegendBackground)=='character'",
                             
                             "!is.na(input$scenarioLegendSize)",
                             "input$scenarioLegendSize!=''",
                             "class(input$scenarioLegendSize) %in% c('numeric','integer')")
  interactiveSetting$message<-c("Please select an Output Map Type",
                                "Please select an Output Map Type",
                                
                                "Please specify a character string for the Scenario Name",
                                "Please specify a character string for the Scenario Name",
                                
                                paste("Scenario : ",input$scenarioName," Exists.  Input new scenario name OR check 'Overwrite'",sep=""),
                                
                                "Please select Source(s) for source adjustment reduction",
                                
                                "Please specify a numeric vector of source reduction factors",
                                "Please specify a numeric vector of source reduction factors",
                                "Please specify a numeric vector of source reduction factors",
                                "Number of Selected Sources MUST match the number of Source adjustment reduction (or increase) factors",
                                
                                "Please specify a set of Map Colors to set breakpoints",
                                "Please specify a set of Map Colors to set breakpoints",
                                "Please specify >2 Map Color to set breakpoints, where the first color indicates No Change",
                                "Map Colors must be selected as a vector of character strings",
                                
                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                "Class Rounding must be an integer value",
                                
                                "Please specify a numeric value for Line Width",
                                "Please specify a numeric value for Line Width",
                                "Line Width must be a numeric value",
                                
                                "Please specify a numeric value for Title Size",
                                "Please specify a numeric value for Title Size",
                                "Title Size must be a numeric value",
                                
                                "Please specify a character value for Map Background",
                                "Please specify a character value for Map Background",
                                "Map Background must be a character value",
                                
                                "Please specify a character value for Legend Background",
                                "Please specify a character value for Legend Background",
                                "Legend Background must be a character value",
                                
                                "Please specify a numeric value for Legend Size",
                                "Please specify a numeric value for Legend Size",
                                "Legend Size must be a numeric value")
  return(interactiveSetting)
}