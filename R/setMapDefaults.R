#'@title setMapDefaults
#'@description Sets default values for any missing required mapping settings and assigns default value to global environment
#add parameters

setMapDefaults<-function(outputERSImaps,
                         
                         #prediction map settings
                         predictionTitleSize,
                         predictionLegendSize,
                         predictionLegendBackground,
                         predictionMapColors,
                         predictionClassRounding,
                         predictionMapBackground,
                         lineWidth,
                         
                         #residual maps settings
                         residual_map_breakpoints,
                         ratio_map_breakpoints,
                         residualTitleSize,
                         residualLegendSize,
                         residualColors,
                         residualPointStyle,
                         residualPointSize_breakpoints,
                         residualPointSize_factor,
                         residualMapBackground,
                         
                         #siteAttribute maps settings
                         siteAttrTitleSize,
                         siteAttrLegendSize,
                         siteAttrColors,
                         siteAttrClassRounding,
                         siteAttr_mapPointStyle,
                         siteAttr_mapPointSize,
                         siteAttrMapBackground,
                         
                         #scenarios
                         scenarioMapColors,
                         
                         #diagnostics
                         diagnosticPlotPointSize,
                         diagnosticPlotPointStyle
                         ){
 
  #must be given in alphabetical order like ls()
  defaults<-list(diagnosticPlotPointSize = 0.6,
                 diagnosticPlotPointStyle = 16,
    lineWidth = 0.8,
                 outputERSImaps = c("no","no","no","no"),
                 predictionClassRounding = 1,
                 predictionLegendBackground = "grey",
                 predictionLegendSize = 0.6,
                 predictionMapBackground = "white",
                 predictionMapColors = c("blue","dark green","gold","red","dark red"),
                 predictionTitleSize = 1,
                 ratio_map_breakpoints = c(0.3,0.5,0.8,1,1.25,2,3.3),
                 residual_map_breakpoints = c(-2.5,-0.75,-0.25,0,0.25,0.75,2.5),
                 residualColors = c("red","red","gold","gold","dark green","dark green","blue","blue"),
                 residualLegendSize = 1,
                 residualMapBackground = "grey",
                 residualPointSize_breakpoints = c(0.75,0.5,0.4,0.25,0.25,0.4,0.5,0.75),
                 residualPointSize_factor = 1,
                 residualPointStyle = c(2,2,1,1,1,1,6,6),
                 residualTitleSize = 1,
                 scenarioMapColors = c("light blue","blue","dark green","gold","red","dark red"),
                 siteAttr_mapPointSize = 1,
                 siteAttr_mapPointStyle = 16,
                 siteAttrClassRounding = 2,
                 siteAttrColors = c("blue","green4","yellow","orange","red","darkred"),
                 siteAttrLegendSize = 1,
                 siteAttrMapBackground = "grey",
                 siteAttrTitleSize = 1
    
  )
  
  listSettings<-ls() 
  for (s in listSettings){
    setting<-get(s)
    if (length(setting)==1){
    if (is.na(setting)){
      assign(s,defaults[s][[1]],envir = .GlobalEnv)
   
    value<-defaults[s][[1]]
    if (length(value)>1){
      value<-paste("c(",paste(value, collapse = ","),")",sep="")
    }
warn<-paste("MISSING VALUE FOR REQUIRED SETTING : ", s, "\n DEFAULT VALUE :  ",value, " USED \n \n",sep="")
message(warn)
 }
    }
  }
}