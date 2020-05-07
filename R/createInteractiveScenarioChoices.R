#'@title createInteractiveScenarioChoices
#'@description Function generates list of prediction metrics for source reduction scenario to plot in RShiny
#'@return data.frame of parameter metrics by type (load and yield) with definitions


createInteractiveScenarioChoices<-function(){
  
  # Load predictions:
  #   pload_total                Total load (fully decayed)
  #   mpload_total               Monitoring-adjusted total load (fully decayed)
  #   pload_nd_total             Total load delivered to streams (no stream decay)
  #   pload_inc                  Total incremental load delivered to streams
  #   pload_inc_deliv            Total incremental load delivered to terminal reach

  # Yield predictions:
  #   Concentration              Concentration based on decayed total load and discharge
  #   yield_total                Total yield (fully decayed)
  #   myield_total               Monitoring-adjusted total yield (fully decayed)
  #   yield_inc                  Total incremental yield delivered to streams
  #   yield_inc_deliv            Total incremental yield delivered to terminal reach


  #load metrics
  choices<-c("pload_total",
             "mpload_total",
             "pload_nd_total",
             "pload_inc",
             "pload_inc_deliv")

  definitions<-c("Total load (fully decayed)",
                 "Monitoring-adjusted total load (fully decayed)",
                 "Total load delivered to streams (no stream decay)",
                 "Total incremental load delivered to streams",
                 "Total incremental load delivered to terminal reach")

  choices<-data.frame(category = rep("Load",length(choices)),
                      variable = choices,
                      definition = definitions)
  Choices<-choices
  
  #yield metrics
  choices<-c("concentration",
             "yield_total",
             "myield_total",
             "yield_inc",
             "yield_inc_deliv")
  definitions<-c("Concentration based on decayed total load and discharge",
                 "Total yield (fully decayed)",
                 "Monitoring-adjusted total yield (fully decayed)",
                 "Total incremental yield delivered to streams",
                 "Total incremental yield delivered to terminal reach")

  
  choices<-data.frame(category = rep("Yield",length(choices)),
                      variable = choices,
                      definition = definitions)
  Choices<-rbind(Choices,choices)

  
  return(Choices) 
  
}