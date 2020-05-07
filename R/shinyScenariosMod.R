shinyScenariosMod<-function(input, output, session, scenarioRtables){

   #get criteria inputs
  observe({
    if (input$domain=="selected reaches"){
  if (input$allSrc=="yes"){
    callModule(handsOnMod, "nsSourceRed", DF = as.data.frame(scenarioRtables$sourceRed))
  callModule(handsOnMod, "nsAllSources", DF = as.data.frame(scenarioRtables$allSourcesDF))
  }else{
    callModule(handsOnMod, "nsAllSourcesNO", DF = as.data.frame(scenarioRtables$allSourcesDFno))
    
  }
    }else{
        callModule(handsOnMod, "nsSourceRedALL", DF = as.data.frame(scenarioRtables$sourceRed))

    }
    callModule(handsOnMod, "nsCosmetic", DF = as.data.frame(scenarioRtables$cosmeticScen))
})

}