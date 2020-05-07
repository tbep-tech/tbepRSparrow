addSourceUI<-function(id, input){
  ns<-NS(id)
  fluidPage(
  h4("Select Sources and Percent Reductions"),
  fluidRow(column(width = 8,offset = 0,style='padding-right:2px;padding-bottom:0px',align="center",
                  selectInput(inputId = ns("Allsource0"),label = HTML("Source"),
                              selected = "",choices = c("","point","ndep","MANC_N","FARM_N"))),
           column(width = 4,offset = 0,style='padding-right:10px;padding-bottom:0px',align="center",
                  textInput(inputId = ns("Allreduction0"),label = "Percent Reduction",
                            ""))),
  uiOutput(ns("sourceInputs")),
  actionButton(ns("addSource"),"Add Source"),
  actionButton(ns("rmSource"), "Remove Source")
 )
}