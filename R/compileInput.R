#'@title compileInput
#'@description Shiny server function updates compiles input from namespace indicated in callModule
#'@param input Shiny input selections
#'@param output Shiny output
#'@param session Shiny session
#'@return invalue a list of all elements in namespace with the exception of button values like selectAll, clearAll

compileInput<-function(input, output, session){

  invalue<-list()
  for (n in names(input)){
    if (!n %in% c("selectAll","clearAll","dropdown")){
    eval(parse(text = paste0("invalue$`",n,"`<-input$`",n,"`")))
  }}
  

  return(invalue)
} 