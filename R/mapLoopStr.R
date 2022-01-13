mapLoopStr<-function(mapType,mapLoopInput.list){
  aggFuncs<-c("mean","min","max","median")
  unPackList(lists = list(mapLoopInput.list = mapLoopInput.list),
             parentObj = list(NA))

if (Rshiny & mapType=="site"){
  # create global variable from list names (mapping.input.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list),
             parentObj = list(NA)) 
  
  enable_plotlyMaps<-as.character(input$enablePlotly)
  add_plotlyVars<-as.character(input$plotlyDrop)
  map_years<-as.character(input$yearSelect)

  if (length(map_years)!=0 & !map_years[1] %in% c("mean","median","min","max")){
    map_years<-as.numeric(map_years)
  }else if (length(map_years)==0){
    map_years<-NA
  }
  map_seasons<-as.character(input$seasonSelect)
  if (length(map_seasons)==0){
    map_seasons<-NA
  }
  mapPageGroupBy<-NA

}
  
  if (mapType %in% c("site","resid")){
    existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
  }
  
  map_loop.list<-list(0)

  for (i in unique(plots$plotKey)){
    usedColors<-character(0)
    if (nrow(plots)>1){
      plotSub<-plots[plots$plotKey==i,]
    }else{
      plotSub<-plots
    }

  for (j in 1:nrow(plotSub)){
    if (is.na(mapPageGroupBy) & is.na(map_years) & is.na(map_seasons)){#all
      dmapfinal$year<-rep(1,nrow(dmapfinal))
      dmapfinal$season<-rep(1,nrow(dmapfinal))
      y<-unique(dmapfinal$year)
      s<-unique(dmapfinal$season)
    }else if (mapPageGroupBy %in% (c("year",NA)) & is.na(map_seasons) & !is.na(map_years)){
      y<-plotSub[j,]$year
      dmapfinal$season<-rep(1,nrow(dmapfinal))
      s<-unique(dmapfinal$season)
    }else if (mapPageGroupBy %in% (c("season",NA)) & is.na(map_years) & !is.na(map_seasons)){
      dmapfinal$year<-rep(1,nrow(dmapfinal))
      y<-unique(dmapfinal$year)
      s<-plotSub[j,]$season
    }else{
      y<-plotSub[j,]$year
      s<-plotSub[j,]$season 
    }
    
   
    if (!is.na(y[1]) & !is.na(s[1])){
      plotdata<-dmapfinal[dmapfinal$year %in% c(y) & dmapfinal$season %in% c(s),]

      if (mapType %in% c("catchment","stream")){
      if ((is.na(map_years) & is.na(map_seasons)) | (!is.na(map_years) & map_years %in% aggFuncs) | (!is.na(map_seasons) & map_seasons %in% aggFuncs)){
        plotdata <- merge(plotShape, plotdata, by.x = commonvar, by.y = commonvar)
      }else{
        plotdata <- merge(plotShape, plotdata, by.x = commonvar, by.y = "mapping_waterid")
        plotdata$mapping_waterid<-eval(parse(text=paste0("plotdata$",commonvar)))

      }
      
   
      if (!mapScenarios){
        if (!is.na(mapunits.list[k])){
         titleStr<-paste0(master_map_list[k],"\n",mapunits.list[k]) 
        }else{
          titleStr<-paste0(master_map_list[k]) 
        }
        
      }else{
        if (!Rshiny){
          if (!is.na(mapunits.list[k])){
          titleStr<-paste(scenario_name,scenario_map_list[k],"\n",mapunits.list[k],sep=" ")
          }else{
            titleStr<-paste(scenario_name,scenario_map_list[k])
          }
        }else{
          if (!is.na(mapunits.list[k])){
          titleStr<-paste(input$scenarioName,master_map_list[k],"\n",mapunits.list[k],sep=" ")
          }else{
            titleStr<-paste(input$scenarioName,master_map_list[k]) 
          }
        }
      }
      }else if (mapType=="site"){
        if (Rshiny){
        if (input$batch!="Batch"){
          mapColumn<-as.character(input$var)
        }else{
          mapColumn<-as.character(attr)
        }
        }

        mapColumnName<-mapColumn 
        titleAttr<-data_names[which(data_names$sparrowNames==mapColumn),]
        unitAttr<-titleAttr$varunits
        titleStr<-paste0(mapColumnName,"\n",unitAttr)

        }else{#resid maps
          titleStr<-strTitle
          
          if (regexpr("Resid",mapColumn,ignore.case = TRUE)>0 ){
            threshold<-0
          }else{
            threshold<-1
          }
          if ("threshold-above" %in% map.list){ 
            above <- eval(parse(text=paste0("plotdata[(plotdata$",mapColumn,"<",threshold,"),]")))  # over predictions (neg residuals)
            nabove <- eval(parse(text=paste0("length(above$",mapColumn,")")))
            titleStr<-paste(strTitle," - Over Predictions - n=",nabove) 
          } 
          }#resid
      

      if(is.na(map_years) & is.na(map_seasons)){
        subTitle<-""                
      }else if (!is.na(map_years) & map_years %in% aggFuncs & !is.na(map_seasons) & map_seasons %in% aggFuncs){
        if (is.na(map_years)){
          titleStr<-paste(map_seasons,titleStr)
        }else{
          titleStr<-paste(map_years,titleStr)
        }
        subTitle<-titleStr
      }else if (is.na(map_seasons) | map_seasons %in% aggFuncs){
        if (map_seasons %in% aggFuncs){
          titleStr<-paste(map_seasons,titleStr)
        }
        
        if ((is.na(map_seasons) & map_years %in% aggFuncs) | (is.na(map_years) & map_seasons %in% aggFuncs)){
          subTitle<-"" 
        }else{
          subTitle<-y
        }
        
      }else if (is.na(map_years) | map_years %in% aggFuncs){
        if (map_years %in% aggFuncs){
          titleStr<-paste(map_years,titleStr)
        }
        if (is.na(map_years) & map_seasons %in% aggFuncs | (is.na(map_seasons) & map_years %in% aggFuncs)){
          subTitle<-"" 
        }else{
          subTitle<-s
        }
        
      }else{
        subTitle<-paste(y,s)
      }
      
      if (mapType %in% c("catchment","stream")){
      if (mapScenarios){
        subTitle<-paste0("base ",subTitle," waterids")
      }
      }

      a <- list(
        text = paste0("<b>",subTitle,"</b>"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1.00,
        showarrow = FALSE,
        font = list(size = 14)
      )
      
      if (is.na(mapPageGroupBy) & !is.na(map_years) & map_years %in% aggFuncs & !is.na(map_seasons) & map_seasons %in% aggFuncs){
        plotPageData<-dmapfinal
      }else if (is.na(mapPageGroupBy) & !is.na(map_seasons) & map_seasons %in% aggFuncs){
        plotPageData<-dmapfinal[dmapfinal$year %in% plotSub$year,]
      }else if (is.na(mapPageGroupBy) & !is.na(map_years) & map_years %in% aggFuncs){
        plotPageData<-dmapfinal[dmapfinal$season %in% plotSub$season,]
      }else{
        plotPageData<-dmapfinal[dmapfinal$year %in% plotSub$year & dmapfinal$season %in% plotSub$season,]
      }
      
      if (mapType %in% c("catchment","stream")){
      pageColors<-unique(plotPageData$color)
      }
 
      
      if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){ 
      p<-plot_ly() %>%
        layout(annotations=a,
               margin =list(t=100),
               xaxis = list(range = lon_limit,
                            showticklabels= TRUE,
                            title = "Longitude"),
               yaxis = list(range = lat_limit,
                            showticklabels = TRUE,
                            title = "Latitude"))
      
      }
      
 
      if (existGeoLines){
        
        if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
          if (mapType!="resid"){
           p <- ggplot() +
            geom_sf(data = GeoLines, size = 0.1, fill = predictionMapBackground, colour ="black") +
            theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_blank())  
          }else{
            p <- ggplot() +
              geom_sf(data = GeoLines, size = 0.1, fill = residualMapBackground, colour ="black") +
              theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(), axis.line = element_blank()) 
          }
          
        }else if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){
          if (mapType!="resid"){
        p <- p %>% add_sf(data = GeoLines,  mode = "lines", type = "scatter",
                          stroke = I("black"),color = I(predictionMapBackground),
                          name = LineShapeGeo, hoverinfo = "none", showlegend=FALSE)
          }else{
            p <- p %>% add_sf(data = GeoLines,  mode = "lines", type = "scatter",
                              stroke = I("black"),color = I(residualMapBackground),
                              name = LineShapeGeo, hoverinfo = "none", showlegend=FALSE)
          }
        
        }
        
        
      }

      if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
        if (nrow(plotSub[!is.na(plotSub$year),])>1 & mapType!="resid"){ 
          legendPos<-c(0.1,0.9)
          legendJus<-"left"
        }else{
          legendPos<-'right'
          legendJus<-"top"
        }
        #function to get legend from plot object
        g_legend<-function(a.gplot){
          tmp <- ggplot_gtable(ggplot_build(a.gplot))
          leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
          legend <- tmp$grobs[[leg]]
          return(legend)}
      }

      if (mapType %in% c("catchment","stream")){
      p.list<-predictMaps_single(mapType,mapLoopInput.list, p, plotdata,plotPageData,titleStr,subTitle,
                                 legendPos,legendJus,usedColors,mapvarname,i)
      }else if (mapType=="site"){
        
        p.list<-mapSiteAttributes(#Rshiny
          input,attr, path_gis, sitedata, LineShapeGeo,data_names,Rshiny,
          #regular
          mapColumn,mapdata=plotdata,GeoLines,mapping.input.list,
          strTitle=titleStr,unitAttr,add_plotlyVars,
          legendPos,legendJus,subTitle,p,plotPageData,usedColors,
          batch_mode)

      }else if (mapType=="resid"){
        p.list<-diagnosticMaps(mapColumn,mapdata=plotdata,GeoLines,
                       map.list,strTitle=titleStr,mapping.input.list,
                       sitedata = sitedata[sitedata$year==y & sitedata$season==s,],p,
                       usedColors,legendPos,legendJus,subTitle)
      }
      
      p<-p.list$p
      usedColors<-p.list$usedColors
      
      if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
      if (j==1){
        mylegend<-g_legend(p)
      }
        if (nrow(plotSub[!is.na(plotSub$year),])>1){
          p<-p %+% theme(legend.position = "none")
        }
      }#static



      eval(parse(text = paste0("p",j,"<-p")))
    }#if plot not missing
  }#j
  
    #create pStr and nrws
    if (nrow(plotSub[!is.na(plotSub$year),])>1){  
      pStr<-paste("p",seq(1,nrow(plotSub[!is.na(plotSub$year),]),1),collapse=",",sep="") 
      
      if (nrow(plotSub[!is.na(plotSub$year),])>2){
        nrws<-2
      }else{
        nrws<-1
      }
    }else{
      pStr<-"p1"
    }

    if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
      
      if (nrow(plots[plots$plotKey==i & !is.na(plots$year),])==4){
        layoutMat<-rbind(c(1,2,5),c(3,4,5)) 
      }else if (nrow(plots[plots$plotKey==i & !is.na(plots$year),])==3){
        layoutMat<-rbind(c(1,2,4),c(3,NA,4)) 
      }else if (nrow(plots[plots$plotKey==i & !is.na(plots$year),])==2){
        layoutMat<-rbind(c(1,2,NA),c(1,2,3)) 
      }

      if (nrow(plotSub[!is.na(plotSub$year),])>1){ 
      eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-gridExtra::arrangeGrob(",pStr,",mylegend, 
                               layout_matrix=layoutMat)")))
      }else{
        eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-gridExtra::arrangeGrob(",pStr,")")))
      }
      
    }else{#plotly or leaflet
      
  if (nrow(plotSub[!is.na(plotSub$year),])>1){
    if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){
    eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-plotly::subplot(",pStr,",nrows = ",nrws,",margin = 0.05) %>% 
                             layout(title=list(text='<b>",titleStr,"</b>',xanchor='right',x=0.9))")))
    }
    
  }else{
    if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){
    #pStr<-eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-p1"))) 
    eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-plotly::subplot(",pStr,",nrows = 1,margin = 0.05) %>% layout(title=list(text='<b>",titleStr,"</b>',xanchor='right',x=0.9))")))
    }else{#leaftlet
      eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-p"))) 
    }
  }
    }#if plotly or leaflet
    
   eval(parse(text=paste0("map_loop.list$p",letters[which(unique(plots$plotKey)==i)],
                          "<-p",letters[which(unique(plots$plotKey)==i)])))
    
   
  }#i  

map_loop.list<-map_loop.list[names(map_loop.list)!=""]


return(map_loop.list)
}