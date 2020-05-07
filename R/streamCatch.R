#'@title streamCatch
#'@description Shiny ui function generates user selections for Stream and Catchment Mapping
#'Uses subroutines: dropFunc. 
#'@param id Shiny namespace designation
#'@param input Shiny input selections
#'@param choices data.frame output of function createInteractiveChoices.R


streamCatch<-function(id, input, choices, map_uncertainties){
  ns <- NS(id)
  
  conditionalPanel(
    condition = "input.mapType == 'Stream' || input.mapType == 'Catchment'",
    
    conditionalPanel(
      condition = "input.batch == 'Batch'",
      h5(HTML("<strong>Select Mapping Variables</strong>")),  

      #batch mapping variables
lapply(1:length(as.character(unique(choices$category))), function(c) {
  category<-as.character(unique(choices$category))[c]
  if (category!="Prediction Uncertainties"){
    nsName<-paste0("ns",tolower(str_split(category," ")[[1]][1]),"Drop")
  }else{
    nsName<-"nsuncertaintyDrop"
  }
  dropFunc(nsName,category,choices)
  })
),

 
#interactive mapping variable
 conditionalPanel(
   condition = "input.batch != 'Batch'",
   selectInput(ns("mapCategory"), "Mapping Variable Type", c("",as.character(unique(choices$category)))),
   selectInput(ns("var"), "Mapping Variable", c("",as.character(choices$variable))),
   textOutput(ns("definition"))
 ),

 #horizontal line
  h5(HTML('<hr style="color: #000000;background-color: #000000; height: 2px"/>')),

 #cosmetic mapping controls
h4("Mapping Settings"),
handsOnUI(ns("nsCosmetic"),input)
 # textInput(ns("predictionMapColors"),"Map Colors (number of colors = number of breakpoints)",paste("c(",paste("'",mapping.input.list$predictionMapColors,"'",collapse=",",sep=""),")",sep="")),
#  h5("For color options click", a("here",href="http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf")),
  
 # textInput(ns("predictionClassRounding"),"Class Rounding (number of decimal points)",as.numeric(mapping.input.list$predictionClassRounding)),
  
 #conditionalPanel(
#   condition = "input.mapType == 'Stream'",
 #  textInput(ns("lineWidth"),"Line Width",as.numeric(mapping.input.list$lineWidth))
 #),          
 #textInput(ns("predictionTitleSize"),"Title Size",as.numeric(mapping.input.list$predictionTitleSize)),
 
 #textInput(ns("predictionMapBackground"),"Map Background Color",paste("'",mapping.input.list$predictionMapBackground,"'",sep="")),
 #textInput(ns("predictionLegendBackground"),"Legend Background Color",paste("'",mapping.input.list$predictionLegendBackground,"'",sep="")),
 #textInput(ns("predictionLegendSize"),"Legend Size",as.numeric(mapping.input.list$predictionLegendSize))
 
  
  )
  
}