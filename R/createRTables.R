createRTables<-function(selectSources,data_names, mapping.input.list){
  #set up Rtables
  #for different reach selection for different sources
  allSourcesDFno<-data.frame(Source = factor(as.character(""),levels = c("",selectSources)),
                             PercentReduction = as.character(""),
                             SelectionVariable = factor(as.character(""),levels = c("",as.character(data_names$sparrowNames))),
                             Min = as.character(""),
                             Max = as.character(""),
                             Equals = as.character(""),
                             Like = as.character(""),
                             Separator = factor(as.character(""),levels = c("","OR","AND")),
                             stringsAsFactors = FALSE )
 
  #for all sources same reach selection
  sourceRed<-data.frame(Source = factor(as.character(""),levels = c("",selectSources)),
                        PercentReduction = as.character(""),
                        stringsAsFactors = FALSE )
  allSourcesDF<-allSourcesDFno[,which(!names(allSourcesDFno) %in% names(sourceRed))]
   
  
  #cosmetic
  cosmetic<-mapping.input.list[which(regexpr("prediction",names(mapping.input.list))>0 |
                                       regexpr("siteAttr",names(mapping.input.list))>0 | 
                                       regexpr("scenario",names(mapping.input.list))>0 |
                                       names(mapping.input.list)=="lineWidth")]
  cosmetic<-cosmetic[which(names(cosmetic)!="map_siteAttributes.list")]
  cosmeticDF<-data.frame(type = character(0),
                         setting = character(0),
                         settingValue = character(0))
  for (c in names(cosmetic)){
    setting<-eval(parse(text = paste0("cosmetic$",c)))
    sub<-data.frame(type = ifelse(regexpr("prediction",c)>0 | c=="lineWidth","prediction",
                                  ifelse(regexpr("site",c)>0,"siteAttr",
                                         "scenario")))
    sub$setting<-c
    sub$settingValue<-ifelse(length(setting)!=1,
                             paste("c(",paste("'",setting,"'",collapse=",",sep=""),")",sep=""),
                            #paste("'",setting,"'",sep=""))
                            setting)
    
    cosmeticDF<-rbind(cosmeticDF,sub)
  }
  
  cosmeticPred<-cosmeticDF[which(cosmeticDF$type=="prediction"),which(names(cosmeticDF)!="type")]
  cosmeticSite<-cosmeticDF[which(cosmeticDF$type=="siteAttr"),which(names(cosmeticDF)!="type")]
  cosmeticScen<-cosmeticDF[which(cosmeticDF$type %in% c("prediction","scenario") & cosmeticDF$setting!="predictionMapColors"),which(names(cosmeticDF)!="type")]
  
  
  #output all tables
  out<-named.list(allSourcesDFno,sourceRed,allSourcesDF,cosmeticPred,cosmeticSite,cosmeticScen)

   return(out)
}