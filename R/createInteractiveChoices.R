#'@title createInteractiveChoices
#'@description Function generates list of prediction metrics to plot in RShiny based on modelled parameters
#'@param SelParmValues selected parameters from parameters.csv using condition `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`
#'@return data.frame of parameter metrics by type (load, yield, uncertainty) with definitions

createInteractiveChoices<-function(SelParmValues,existPredict,subdata, data_names, map_uncertainties){
  if (existPredict==TRUE){
  sources<-as.character(SelParmValues$srcvar)
  
  # Load predictions:
  #   pload_total                Total load (fully decayed)
  #   pload_(sources)            Source load (fully decayed)
  #   mpload_total               Monitoring-adjusted total load (fully decayed)
  #   mpload_(sources)           Monitoring-adjusted source load (fully decayed)
  #   pload_nd_total             Total load delivered to streams (no stream decay)
  #   pload_nd_(sources)         Source load delivered to streams (no stream decay)
  #   pload_inc                  Total incremental load delivered to streams
  #   pload_inc_(sources)        Source incremental load delivered to streams
  #   deliv_frac                 Fraction of total load delivered to terminal reach
  #   pload_inc_deliv            Total incremental load delivered to terminal reach
  #   pload_inc_(sources)_deliv  Source incremental load delivered to terminal reach
  #   share_total_(sources)      Source shares for total load (percent)
  #   share_inc_(sources)        Source share for incremental load (percent)
  
  # Yield predictions:
  #   Concentration              Concentration based on decayed total load and discharge
  #   yield_total                Total yield (fully decayed)
  #   yield_(sources)            Source yield (fully decayed)
  #   myield_total               Monitoring-adjusted total yield (fully decayed)
  #   myield_(sources)           Monitoring-adjusted source yield (fully decayed)
  #   yield_inc                  Total incremental yield delivered to streams
  #   yield_inc_(sources)        Source incremental yield delivered to streams
  #   yield_inc_deliv            Total incremental yield delivered to terminal reach
  #   yield_inc_(sources)_deliv  Source incremental yield delivered to terminal reach
  
  # Uncertainty predictions (requires prior execution of bootstrap predictions):
  #   se_pload_total             Standard error of the total load (percent of mean)
  #   ci_pload_total             95% prediction interval of the total load (percent of mean)
  
#load metrics
  choices<-c("pload_total",
             paste("pload_",sources,sep=""),
             "mpload_total",
             paste("mpload_",sources,sep=""),
             "pload_nd_total",
             paste("pload_nd_",sources,sep=""),
             "pload_inc",
             paste("pload_inc_",sources,sep=""),
             "deliv_frac",
             "pload_inc_deliv",
             paste("pload_inc_",sources,"_deliv",sep=""),
             paste("share_total_",sources,sep=""),
             paste("share_inc_",sources,sep=""))
  
  definitions<-c("Total load (fully decayed)",
                 rep("Source load (fully decayed)",length(sources)),
                 "Monitoring-adjusted total load (fully decayed)",
                 rep("Monitoring-adjusted source load (fully decayed)",length(sources)),
                 "Total load delivered to streams (no stream decay)",
                 rep("Source load delivered to streams (no stream decay)",length(sources)),
                 "Total incremental load delivered to streams",
                 rep("Source incremental load delivered to streams",length(sources)),
                 "Fraction of total load delivered to terminal reach",
                 "Total incremental load delivered to terminal reach",
                 rep("Source incremental load delivered to terminal reach",length(sources)),
                 rep("Source shares for total load (percent)",length(sources)),
                 rep("Source share for incremental load (percent)",length(sources)))
  
  choices<-data.frame(category = rep("Load Predictions",length(choices)),
                      variable = choices,
                      definition = definitions)
  Choices<-choices
  
#yield metrics
  choices<-c("concentration",
             "yield_total",
             paste("yield_",sources,sep=""),
             "myield_total",
             paste("myield_",sources,sep=""),
             "yield_inc",
             paste("yield_inc_",sources,sep=""),
             "yield_inc_deliv",
             paste("yield_inc_",sources,"_deliv",sep=""))
  definitions<-c("Concentration based on decayed total load and discharge",
    "Total yield (fully decayed)",
    rep("Source yield (fully decayed)",length(sources)),
    "Monitoring-adjusted total yield (fully decayed)",
    rep("Monitoring-adjusted source yield (fully decayed)",length(sources)),
    "Total incremental yield delivered to streams",
    rep("Source incremental yield delivered to streams",length(sources)),
    "Total incremental yield delivered to terminal reach",
    rep("Source incremental yield delivered to terminal reach",length(sources)))
                 
                 
  choices<-data.frame(category = rep("Yield Predictions",length(choices)),
                      variable = choices,
                      definition = definitions)
  Choices<-rbind(Choices,choices)
  
  if (!is.na(map_uncertainties)){
  #Uncertainty metrics
  choices<-c("se_pload_total",
             "ci_pload_total",
             "se_mpload_total",
             "ci_mpload_total",
             "model.error.var",
             "sample.error.var.boots")
  definitions<-c("Standard error of the total load (percent of mean)",
    "95% prediction interval of the total load (percent of mean)",
    "",
    "",
    "",
    "")
    
  choices<-data.frame(category = rep("Prediction Uncertainties",length(choices)),
                      variable = choices,
                      definition = definitions)
  Choices<-rbind(Choices,choices)
  }
  #data dictionary variables
  for (n in names(subdata)[which(regexpr("waterid",names(subdata))<0)]){
   # sub<-na.omit(subdata[,n])
  #  if (length(sub)==nrow(subdata)){
      if (class(subdata[,n])=="numeric"){
      def<-data_names[which(data_names$sparrowNames==n),]$explanation
      if (length(def)==0){
        def<-""
      }
      choices<-data.frame(category = c("Data Dictionary Variable"),
                          variable = n,
                          definition = def)
      Choices<-rbind(Choices,choices)
    }
  }
  
  }else{#only data dictionary variables
    Choices<-data.frame(matrix(0, nrow= 0, ncol = 3))
    names(Choices)<-c("category","variable","definition")
  for (n in names(subdata)[which(regexpr("waterid",names(subdata))<0)]){
    #sub<-na.omit(subdata[,n])
    #if (length(sub)==nrow(subdata)){
    if (class(subdata[,n])=="numeric"){
      def<-data_names[which(data_names$sparrowNames==n),]$explanation
      if (length(def)==0){
        def<-""
      }
      choices<-data.frame(category = c("Data Dictionary Variable"),
                          variable = n,
                          definition = def)
      Choices<-rbind(Choices,choices)
    }
  }

}
  return(Choices) 
 
}