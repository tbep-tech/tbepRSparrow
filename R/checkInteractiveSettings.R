#'@title checkInteractiveSettings
#'@description Checks for invalid user input in Shiny app.  
#'If invalid settings are found a modal dialog box is triggered with a message to the user to update the setting
#'@param input interactive user input in Shiny app
#'@param path_results path to results directory
#'@return logical TRUE/FALSE indicating whether or not invalid settings were found

checkInteractiveSettings<-function(input,path_results){


if (input$mapType %in% c("Stream","Catchment")){
  interactiveSetting<-interactivePredictionSettings(input)
}else if (input$mapType=="Site Attributes"){
    interactiveSetting<-interactiveSiteSettings(input)
}else if (input$mapType=="Source Reduction Scenarios"){
  if (input$batch!="Batch"){
  #  interactiveSetting<-interactiveScenarioSettings(input)
  }else{
   # interactiveSetting<-batchScenarioSettings(input)
  }
}


  #check settings
badSetting<-FALSE
if (input$mapType!="Source Reduction Scenarios"){
for (s in 1:length(interactiveSetting$test)){
  if (badSetting==FALSE){
    tryTest<-try({eval(parse(text=interactiveSetting$test[s]))},TRUE)
    if (class(tryTest)=="try-error"){
      badSetting<-TRUE
      showModal( modalDialog(
        title = "",
        HTML(interactiveSetting$example[s]),
        footer = tagList(
          modalButton("OK")
        )
      ))
    }else if (eval(parse(text=interactiveSetting$test[s]))==FALSE){
    badSetting<-TRUE
    showModal( modalDialog(
      title = interactiveSetting$message[s],
      footer = tagList(
        modalButton("OK")
      )
    ))
    
  }
  }
}
}
return(badSetting)

}#end function