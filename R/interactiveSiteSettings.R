#'@title interactiveSiteSettings
#'@description Generates list of site attribute map setting checks and error messages for Rshiny in Interactive mode
#'@param input interactive user input in Shiny app
#'@return interactiveSetting a list object with test=setting tests and message = error messages

interactiveSiteSettings<-function(input){
  interactiveSetting<-list()
  if (input$batch=="Batch"){
    mapVar<-"input$dataCheck"
    setMessage<-"Please select a Site Attribute"
    setExample<-setMessage
  }else{
    mapVar<-c("!is.na(input$var)",
                "input$var!=''")
    setMessage<-c("Please select a Site Attribute",
                  "Please select a Site Attribute")
    setExample<-setMessage
  }
  
  interactiveSetting$test<-c(mapVar,
                             
                             "!is.na(input$siteAttrColors)",
                             "input$siteAttrColors!=''",
                             "length(eval(parse(text=input$siteAttrColors)))>1",
                             "class(eval(parse(text=input$siteAttrColors)))=='character'",
                             "areColors(eval(parse(text=input$siteAttrColors)))",
                             
                             "!is.na(input$siteAttrClassRounding)",
                             "input$siteAttrClassRounding!=''",
                             "suppressWarnings(!is.na(as.numeric(input$siteAttrClassRounding)))",
                             "round(eval(parse(text=input$siteAttrClassRounding)))==input$siteAttrClassRounding",

                             "!is.na(input$siteAttr_mapPointStyle)",
                             "input$siteAttr_mapPointStyle!=''",
                             "suppressWarnings(!is.na(as.numeric(input$siteAttr_mapPointStyle)))",
                             "round(eval(parse(text=input$siteAttr_mapPointStyle)))==input$siteAttr_mapPointStyle",
                             
                             "!is.na(input$size)",
                             "input$size!=''",
                             "suppressWarnings(!is.na(as.numeric(input$size)))",
                             
                             "!is.na(input$siteAttrTitleSize)",
                             "input$siteAttrTitleSize!=''",
                             "suppressWarnings(!is.na(as.numeric(input$siteAttrTitleSize)))",
                             
                             "!is.na(input$siteAttrMapBackground)",
                             "input$siteAttrMapBackground!=''",
                             "class(eval(parse(text =input$siteAttrMapBackground)))=='character'",
                             "areColors(eval(parse(text = input$siteAttrMapBackground)))",
                             
                             
                             "!is.na(input$siteAttrLegendSize)",
                             "input$siteAttrLegendSize!=''",
                             "suppressWarnings(!is.na(as.numeric(input$siteAttrLegendSize)))"
                             )
                             
  interactiveSetting$message<-c(setMessage,
                                
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
  
  interactiveSetting$example<-c(setExample,
                                
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
  
  
  
  
  return(interactiveSetting)
}