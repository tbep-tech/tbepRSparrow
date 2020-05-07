#'@title batchSiteSettings
#'@description Generates list of site attribute map setting checks and error messages for Rshiny in batch mode
#'@param input interactive user input in Shiny app
#'@return batchSetting a list object with test=setting tests and message = error messages

batchSiteSettings<-function(input){
  batchSetting<-list()
  batchSetting$test<-c("length(input$attrCheck)!=0",
                             
                       "!is.na(input$siteAttrColors_batch)",
                       "input$siteAttrColors_batch!=''",
                       "length(eval(parse(text=input$siteAttrColors_batch)))>1",
                       "class(eval(parse(text=input$siteAttrColors_batch)))=='character'",
                       "areColors(eval(parse(text=input$siteAttrColors_batch)))",
                             
                       "!is.na(input$siteAttrClassRounding_batch)",
                             "input$siteAttrClassRounding_batch!=''",
                       "suppressWarnings(!is.na(as.numeric(input$siteAttrClassRounding_batch)))",
                       "round(eval(parse(text=input$siteAttrClassRounding_batch)))==input$siteAttrClassRounding_batch",
                             
                       "!is.na(input$siteAttr_mapPointStyle_batch)",
                             "input$siteAttr_mapPointStyle_batch!=''",
                       "suppressWarnings(!is.na(as.numeric(input$siteAttr_mapPointStyle_batch)))",
                       "round(eval(parse(text=input$siteAttr_mapPointStyle_batch)))==input$siteAttr_mapPointStyle_batch",
                             
                       "!is.na(input$sizeBatch)",
                             "input$sizeBatch!=''",
                       "suppressWarnings(!is.na(as.numeric(input$sizeBatch)))",
                             
                       "!is.na(input$siteAttrTitleSize_batch)",
                             "input$siteAttrTitleSize_batch!=''",
                       "suppressWarnings(!is.na(as.numeric(input$siteAttrTitleSize_batch)))",
                             
                       "!is.na(input$siteAttrMapBackground_batch)",
                             "input$siteAttrMapBackground_batch!=''",
                       "class(eval(parse(text =input$siteAttrMapBackground_batch)))=='character'",
                       "areColors(eval(parse(text = input$siteAttrMapBackground_batch)))",
                             
                       "!is.na(input$siteAttrLegendSize_batch)",
                             "input$siteAttrLegendSize_batch!=''",
                       "suppressWarnings(!is.na(as.numeric(input$siteAttrLegendSize_batch)))"
  )
  batchSetting$message<-c("Please select a Site Attribute",
                                
                          "Please specify a set of Map Colors to set breakpoints",
                          "Please specify a set of Map Colors to set breakpoints",
                          "Please specify >1 Map Color to set breakpoints",
                          "Map Colors must be selected as a vector of character strings",
                          "Invalid Map Colors selected.  Please select valid color character strings.",
                                
                          "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                          "Class Rounding must be an integer value",
                          "Class Rounding must be an integer value",
                                
                          "Please specify an integer value for Point Style",
                                "Please specify an integer value for Point Style",
                                "Point Style must be an integer value",
                          "Point Style must be an integer value",
                                
                          "Please specify a numeric value for Point Size",
                                "Please specify a numeric value for Point Size",
                                "Point Size must be a numeric value",
                                
                          "Please specify a numeric value for Title Size",
                                "Please specify a numeric value for Title Size",
                                "Title Size must be a numeric value",
                                
                          "Please specify a character value for Map Background",
                                "Please specify a character value for Map Background",
                                "Map Background must be a character value",
                          "Invalid Map Background Color selected.  Please select valid color character string.",
                          
                                
                          "Please specify a numeric value for Legend Size",
                                "Please specify a numeric value for Legend Size",
                                "Legend Size must be a numeric value")
  batchSetting$example<-c("Please select a Site Attribute",
                                
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                "Please specify a set of Map Colors to set breakpoints as vector of colors in single quotes <br> Example : c('blue','dark green','gold','red','dark red')",
                                
                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                "Please specify an integer value for Class Rounding",
                                
                                "Please specify an integer value for Point Style",
                                "Please specify an integer value for Point Style",
                                "Please specify an integer value for Point Style",
                                "Please specify an integer value for Point Style",
                                
                                "Please specify a numeric value for Point Size",
                                "Please specify a numeric value for Point Size",
                                "Please specify a numeric value for Point Size",
                                
                                "Please specify a numeric value for Title Size",
                                "Please specify a numeric value for Title Size",
                                "Please specify a numeric value for Title Size",
                                
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                "Please specify a character value for Map Background as a single string with single quotes<br> Example : 'white'",
                                
                                "Please specify a numeric value for Legend Size",
                                "Please specify a numeric value for Legend Size",
                                "Please specify a numeric value for Legend Size"
  )
  
  
  
  return(batchSetting)
}