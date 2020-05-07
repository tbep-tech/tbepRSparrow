#'@title batchPredictionSettings
#'@description Generates list of prediction map setting checks and error messages for Rshiny in batch mode
#'@param input interactive user input in Shiny app
#'@return batchSetting a list object with test=setting tests and message = error messages

batchPredictionSettings<-function(input){
  batchSetting<-list()
  batchSetting$test<-c(      "length(input$loadCheck)!=0 | length(input$yieldCheck)!=0 | length(input$uncertaintyCheck)!=0",
    
                             "!is.na(input$predictionMapColors)",
                             "input$predictionMapColors!=''",
                             "length(eval(parse(text=input$predictionMapColors)))>1",
                             "class(eval(parse(text=input$predictionMapColors)))=='character'",
                             "areColors(eval(parse(text=input$predictionMapColors)))",
                             
                             "!is.na(input$predictionClassRounding)",
                             "input$predictionClassRounding!=''",
                             "suppressWarnings(!is.na(as.numeric(input$predictionClassRounding)))",
                             "round(eval(parse(text=input$predictionClassRounding)))==input$predictionClassRounding",

                             "if (input$mapType=='Stream'){!is.na(input$lineWidth)
                              }else{TRUE}",
                             "if (input$mapType=='Stream'){input$lineWidth!=''
                              }else{TRUE}",
                             "if (input$mapType=='Stream'){suppressWarnings(!is.na(as.numeric(input$lineWidth)))
                                }else{TRUE}",
                            
                             "!is.na(input$predictionTitleSize)",
                             "input$predictionTitleSize!=''",
                             "suppressWarnings(!is.na(as.numeric(input$predictionTitleSize)))",

                             "!is.na(input$predictionMapBackground)",
                             "input$predictionMapBackground!=''",
                             "class(eval(parse(text =input$predictionMapBackground)))=='character'",
                             "areColors(eval(parse(text = input$predictionMapBackground)))",
                             
                             "!is.na(input$predictionLegendBackground)",
                             "input$predictionLegendBackground!=''",
                             "class(eval(parse(text =input$predictionLegendBackground)))=='character'",
                             "areColors(eval(parse(text = input$predictionLegendBackground)))",

                             "!is.na(input$predictionLegendSize)",
                             "input$predictionLegendSize!=''",
                             "suppressWarnings(!is.na(as.numeric(input$predictionLegendSize)))")
  
  batchSetting$message<-c(      "Please select Prediction Metrics",
    
                                "Please specify a set of Map Colors to set breakpoints",
                                "Please specify a set of Map Colors to set breakpoints",
                                "Please specify >1 Map Color to set breakpoints",
                                "Map Colors must be selected as a vector of character strings",
                                "Invalid Map Colors selected.  Please select valid color character strings.",

                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                "Class Rounding must be an integer value",
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
                                "Invalid Map Background Color selected.  Please select valid color character string.",

                                "Please specify a character value for Legend Background",
                                "Please specify a character value for Legend Background",
                                "Legend Background must be a character value",
                                "Invalid Legend Background Color selected.  Please select valid color character string.",

                                "Please specify a numeric value for Legend Size",
                                "Please specify a numeric value for Legend Size",
                                "Legend Size must be a numeric value")
  
  batchSetting$example<-c("Please select Prediction Metrics",
                                
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                
                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                
                                "Please specify a numeric value for Line Width",
                                "Please specify a numeric value for Line Width",
                                "Please specify a numeric value for Line Width",
                                
                                "Please specify a numeric value for Title Size",
                                "Please specify a numeric value for Title Size",
                                "Please specify a numeric value for Title Size",
                                
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                
                                "Please specify a character value for Legend Background as a single string with single quotes<br> Example : 'grey'",
                                "Please specify a character value for Legend Background as a single string with single quotes<br> Example : 'grey'",
                                "Please specify a character value for Legend Background as a single string with single quotes<br> Example : 'grey'",
                                "Please specify a character value for Legend Background as a single string with single quotes<br> Example : 'grey'",
                                
                                "Please specify a numeric value for Legend Size",
                                "Please specify a numeric value for Legend Size",
                                "Please specify a numeric value for Legend Size"
  )
  
  return(batchSetting)
}