#'@title batchScenarioSettings
#'@description Generates list of scenario map setting checks and error messages for Rshiny in batch mode
#'@param input interactive user input in Shiny app
#'@return batchSetting a list object with test=setting tests and message = error messages

batchScenarioSettings<-function(input){
  batchSetting<-list()
  batchSetting$test<-c(      "length(input$outCheck)==0",
                             
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
                             
                             "!is.na(input$scenarioMapColors_batch)",
                             "input$scenarioMapColors_batch!=''",
                             "length(eval(parse(text=input$scenarioMapColors_batch)))>1",
                             "class(eval(parse(text=input$scenarioMapColors_batch)))=='character'",
                             
                             "!is.na(input$scenarioClassRounding_batch)",
                             "input$scenarioClassRounding_batch!=''",
                             "class(input$scenarioClassRounding_batch)=='integer'",
                             
                             "if (regexpr('Stream',paste(as.character(input$outCheck),collapse=",")>0){!is.na(input$scenariolineWidth_batch)
                              }else{TRUE}",
                             "if (regexpr('Stream',paste(as.character(input$outCheck),collapse=",")>0){input$scenariolineWidth_batch!=''
                              }else{TRUE}",
                             "if (regexpr('Stream',paste(as.character(input$outCheck),collapse=",")>0){class(input$scenariolineWidth_batch) %in% c('numeric','integer')
                             }else{TRUE}",
                            
                             "!is.na(input$scenarioTitleSize_batch)",
                             "input$scenarioTitleSize_batch!=''",
                             "class(input$scenarioTitleSize_batch) %in% c('numeric','integer')",
                             
                             "!is.na(input$scenarioMapBackground_batch)",
                             "input$scenarioMapBackground_batch!=''",
                             "class(input$scenarioMapBackground_batch)=='character'",
                             
                             "!is.na(input$scenarioLegendBackground_batch)",
                             "input$scenarioLegendBackground_batch!=''",
                             "class(input$scenarioLegendBackground_batch)=='character'",
                             
                             "!is.na(input$scenarioLegendSize_batch)",
                             "input$scenarioLegendSize_batch!=''",
                             "class(input$scenarioLegendSize_batch) %in% c('numeric','integer')")
  
  batchSetting$message<-c(      "Please select Output Map Type",
                                
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
                                "Please specify >1 Map Color to set breakpoints",
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
  return(batchSetting)
}