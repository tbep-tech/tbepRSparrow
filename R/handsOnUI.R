handsOnUI<-function(id, input){
  ns<-NS(id)

fluidPage(

  fluidRow( 
    rHandsontableOutput(ns("hot"))
    
  ))
}