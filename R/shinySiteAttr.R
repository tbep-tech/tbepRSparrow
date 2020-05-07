#'@title shinySiteAttr
#'@description Shiny ui function generates user selections for Site Attribute Mapping
#'Uses subroutines: dropFunc. 
#'@param id Shiny namespace designation
#'@param input Shiny input selections
#'@param choices data.frame output of function createInteractiveChoices.R


shinySiteAttr<-function(id, input, choices){
  ns <- NS(id)
  
  conditionalPanel(
    condition = "input.mapType == 'Site Attributes'",
    
    conditionalPanel(
      condition = "input.batch=='Interactive'",
      selectInput(ns("var"), "Site Attribute", 
                  c("",as.character(choices[which(choices$category=="Data Dictionary Variable"),]$variable))),
      textOutput(ns("definition"))),
    
    conditionalPanel(
      condition = "input.batch == 'Batch'",
      h5(HTML("<strong>Select Mapping Variables</strong>")),  
      dropFunc("nsattrDrop","Data Dictionary Variable",choices)),
    
    #horizontal line
    h5(HTML('<hr style="color: #000000;background-color: #000000; height: 2px"/>')),
    #cosmetic mapping controls
    h4("Mapping Settings"),
    handsOnUI(ns("nsCosmetic"),input)
    #cosmetic mapping controls
    #textInput(ns("siteAttrColors"),"Map Colors (number of colors = number of breakpoints)",paste("c(",paste("'",mapping.input.list$siteAttrColors,"'",collapse=",",sep=""),")",sep="")),
    #h5("For color options click", a("here",href="http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf")),
    
  #  textInput(ns("siteAttrClassRounding"),"Class Rounding (number of decimal points)",as.numeric(mapping.input.list$siteAttrClassRounding)),
  #  textInput(ns("siteAttr_mapPointStyle"),"Point Style (pch value)",as.numeric(mapping.input.list$siteAttr_mapPointStyle)),
  #  h5("For point style (pch value) options click", a("here",href="https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/points")),
  #  textInput(ns("size"),"Point Size",as.numeric(mapping.input.list$siteAttr_mapPointSize)),
    
  #  textInput(ns("siteAttrMapBackground"),"Map Background Color",paste("'",mapping.input.list$siteAttrMapBackground,"'",sep="")),
  #  textInput(ns("siteAttrTitleSize"),"Title Size",as.numeric(mapping.input.list$siteAttrTitleSize)),
  #  textInput(ns("siteAttrLegendSize"),"Legend Size",as.numeric(mapping.input.list$siteAttrLegendSize))
  )
}
    