#'@title selectAll
#'@description Shiny server function updates variable drop down list selecting all variable choices in selected namespace given by 'category'
#'@param input Shiny input selections
#'@param output Shiny output
#'@param session Shiny session
#'@param category indicates namespace of variable list to be updated
#'@param choices data.frame output of function createInteractiveChoices.R


selectAll<-function(input, output, session, category, choices){

  if (category!="Prediction Uncertainties"){
   name<-paste0(tolower(str_split(category," ")[[1]][1]),"Check")
}else{
  name<-"uncertaintyCheck"
}
   #select all
   observeEvent(input$selectAll,{
      updateCheckboxGroupInput(session,name,selected = as.character(choices[which(choices$category==category),]$variable))
    })
   #clear all
  observeEvent(input$clearAll,{
    updateCheckboxGroupInput(session,name,choices = as.character(choices[which(choices$category==category),]$variable))
  })

}

