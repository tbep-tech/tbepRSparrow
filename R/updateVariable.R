#'@title updateVariable
#'@description Shiny server function updates variable drop down list according to the 
#'user's selection of mapping variable type and outputs the definition of the variable
#'in Shiny namespace = 'nsStreamCatch'
#'@param input Shiny input selections
#'@param output Shiny output
#'@param session Shiny session
#'@param choices data.frame output of function createInteractiveChoices.R

updateVariable<-function(input,output, session,choices, mapType){
  if (mapType=="Site Attributes"){
    category<-"Data Dictionary Variable"
  }else{
    category<-input$mapCategory
  }
  updateSelectInput(session, "var", 
                    choices = as.character(choices[which(choices$category==category),]$variable))
  output$definition <- renderText({
    as.character(choices[which(choices$variable==input$var),]$definition)
  })
}