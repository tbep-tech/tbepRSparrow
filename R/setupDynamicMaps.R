setupDynamicMaps<-function(dmapfinal,map_years,map_seasons,mapPageGroupBy,mapsPerPage, Rshiny, enable_plotlyMaps){
  aggFuncs<-c("mean","median","min","max")

  #if "all" selected get list
  if (!is.na(map_years) & (map_years=="all" | map_years %in% aggFuncs)){
    map_years<-unique(dmapfinal$year)
  }
  if (!is.na(map_seasons) & (map_seasons=="all" | map_seasons %in% aggFuncs)){
    map_seasons<-unique(dmapfinal$season)
  }

  #subset data by year and season
  if (!is.na(map_years[1]) & !is.na(map_seasons[1])
      & !map_years %in% aggFuncs & !map_seasons %in% aggFuncs){ #map specific years or seasons
    dmapfinal<-dmapfinal[dmapfinal$year %in% map_years & dmapfinal$season %in% map_seasons,]
  }else if (!is.na(map_years[1]) & !map_years %in% aggFuncs){#map specific years
    dmapfinal<-dmapfinal[dmapfinal$year %in% map_years,]
  }else if (!is.na(map_seasons) & !map_seasons %in% aggFuncs){#map specific seasons
    dmapfinal<-dmapfinal[dmapfinal$season %in% map_seasons,]
  }else{
    mapPageGroupBy<-NA #all
  }
  
  plotData<-as.data.frame(dmapfinal)
  

  #create plot sequence
  plotSeq<-numeric(0)


  if (mapPageGroupBy %in% c("year",NA) &
            (!is.na(map_years) & !map_years %in% aggFuncs & !is.na(map_seasons) & !map_seasons %in% aggFuncs)){
    plots<-unique(plotData[c("year","season")])
  }else if (mapPageGroupBy %in% c("year",NA) &
            ((is.na(map_seasons) | map_seasons %in% aggFuncs) & !is.na(map_years) & !map_years %in% aggFuncs)){
    plots<-unique(plotData[c("year")])
    plots$season<-rep(1,nrow(plots))
  }else if (mapPageGroupBy %in% c("season",NA) &
            ((is.na(map_years) | map_years %in% aggFuncs) & !is.na(map_seasons) & !map_seasons %in% aggFuncs)){
    plots<-unique(plotData[c("season")])
    plots$year<-rep(1,nrow(plots))
  }else if (is.na(mapPageGroupBy) & (((is.na(map_years) | map_years %in% aggFuncs) & (is.na(map_seasons) | map_seasons %in% aggFuncs)) |
                                     (is.na(map_years) & is.na(map_seasons)))){#all
    plots<-data.frame(year = 1, season = 1, plotKey = 1)
  }else{
    plots<-unique(plotData[c("year","season")])
  }

  if (is.na(mapPageGroupBy)){
    plotSeq<-rep(seq(1,nrow(plots),mapsPerPage),each=mapsPerPage)
  }
  if (!is.na(mapPageGroupBy)){
  if(mapPageGroupBy=="year"){
    group<-map_years
  }else if (mapPageGroupBy=="season"){
    group<-map_seasons
  }
  }else{
    group<-NA
  }

  if (!is.na(mapPageGroupBy)){
  if (mapPageGroupBy=="year"){
    #plots<-plots[order(plots$year,plots$season),]
    plots <- plots %>% 
      arrange(factor(season, levels = c("winter","spring","summer","fall")))
    plots<-plots[order(plots$year),]
  }else if (mapPageGroupBy=="season"){
    plots<-plots[order(plots$season,plots$year),]
  }
  }else if (!is.na(map_seasons) & (is.na(map_years) | length(map_years)==1)){
    if (length(map_seasons)>1){
      plots <- plots %>% 
        arrange(factor(season, levels = c("winter","spring","summer","fall")))
    }
  }
  

  #fill in blank lines for missing seasons or years
  if(!is.na(group)){
    groupSeq<-numeric(0)
    pItr<-nrow(plots)
    
    for (y in group){
      pItr<-pItr+1
      ydata<-plots
      names(ydata)[names(ydata)==mapPageGroupBy]<-"groupVar"
      uniqueVar<-ifelse(mapPageGroupBy=="year","season",ifelse(mapPageGroupBy=="season","year",NA))
      names(ydata)[names(ydata)==uniqueVar]<-"uniqueVar"
      yseq<-rep(y,length(na.omit(unique(ydata[ydata$groupVar==y,]$uniqueVar))))
      
      if (length(yseq)<mapsPerPage & !is.na(mapPageGroupBy)){
        
        
        addMissingPlots<-plots[1,]
        names(addMissingPlots)<-c("groupVar","uniqueVar")
        addMissingPlots$groupVar<-NA
        addMissingPlots$uniqueVar<-NA
        
        
        for (m in 1:(mapsPerPage-(length(yseq)))){
          ydata2<-plots
          
          names(ydata2)[names(ydata2)==mapPageGroupBy]<-"groupVar"
          uniqueVar<-ifelse(mapPageGroupBy=="year","season",ifelse(mapPageGroupBy=="season","year",NA))
          names(ydata2)[names(ydata2)==uniqueVar]<-"uniqueVar"
          
          
          ydata2<-rbind(ydata2[ydata2$groupVar==y & !is.na(ydata2$groupVar),],addMissingPlots,ydata2[ydata2$groupVar!=y,])
          names(ydata2)<-names(plots)
          
          plots<-ydata2
        }
        
        yseq<-c(yseq,rep(NA,mapsPerPage-(length(yseq))))
        
      }
      if (!is.na(mapPageGroupBy)){
        if (length(yseq)==mapsPerPage){
          pSeq<-rep(pItr,mapsPerPage)
        }else{
          
          pSeq<-rep(seq(pItr,length(yseq)+pItr,1),each=mapsPerPage,length.out=length(yseq))
          
          
        }
        plotSeq<-c(plotSeq,pSeq)
      }
      groupSeq<-c(groupSeq,yseq)
      pItr<-max(pSeq)
    }
  }
  

  #generate plot sequence variable
  if (is.na(mapPageGroupBy) & ((!is.na(map_years) & map_years %in% aggFuncs & !is.na(map_seasons) & map_seasons %in% aggFuncs) |
                               (is.na(map_years) & is.na(map_seasons)))){#all
  }else{
    plotSeq<-plotSeq[1:nrow(plots)]
    plots$plotKey<-plotSeq
  }
  
  if (Rshiny){
    plots<-plots[plots$plotKey==1,]
  }
  if (enable_plotlyMaps=="leaflet"){
    plots<-plots[1,]
  }

 # print(plots)
  return(plots)
}