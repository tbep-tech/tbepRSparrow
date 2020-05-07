compileALL<-function(input, output, session, path_results, choices){
  
  top<-list(batch = input$batch,
            mapType = input$mapType)
  
  nsList<-character(0)
  if (input$batch=="Batch"){
    nsList<-c(nsList,"nsBatch")
  }
  if (input$mapType  %in% c("Stream","Catchment")){
    nsList<-c(nsList,"nsStreamCatch")
    if (input$batch=="Batch"){
      for (c in as.character(unique(choices$category))){
        if (c!="Prediction Uncertainties"){
          nsName<-paste0("ns",tolower(str_split(c," ")[[1]][1]),"Drop")
         
        }else{
          nsName<-"nsuncertaintyDrop"
        }
         nsList<-c(nsList,nsName)
    }
    }
  }else if (input$mapType=="Site Attributes"){
    nsList<-c(nsList, "nsSiteAttr","nsattrDrop")
  }else if (input$mapType=="Source Reduction Scenarios"){
    nsList<-c(nsList, "nsScenarios")
  }
  
  compiledInput<-top
  for (n in nsList){
    compiledInput<-append(compiledInput,callModule(compileInput,n))
  }
  
  
#  compiledInput<-callModule(compileInput,"nsStreamCatch")
#  compiledInput<-append(compiledInput,callModule(compileInput,"nsBatch"))
#  compiledInput<-append(compiledInput,top)
  
 # if (input$batch=="Batch" & input$mapType %in% c("Stream","Catchment")){
#    for (c in as.character(unique(choices$category))){
#      if (c!="Prediction Uncertainties"){
#        nsName<-paste0("ns",tolower(str_split(c," ")[[1]][1]),"Drop")
#      }else{
#        nsName<-"nsuncertaintyDrop"
#      }
#      compiledInput<-append(compiledInput,callModule(compileInput,nsName))
#      
#    }
#  }
  
  #badSetting<-checkInteractiveSettings(compiledInput,path_results)
  
out<-list(compiledInput = compiledInput)#,
   #       badSetting=badSetting)
return(out)
}