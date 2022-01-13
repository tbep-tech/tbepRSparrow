test_addPlotlyvars<-function(add_plotlyVars,subdata,groupVar,maxUnique){
  for (v in add_plotlyVars){
    test<-unique(subdata[c(groupVar,v)])
    if (nrow(test)>maxUnique){
      message(paste0(v," not unique per mapping_waterid removed from add_plotlyVars"))
      add_plotlyVars<-add_plotlyVars[add_plotlyVars!=v]
    }
  }
  return(add_plotlyVars)
}