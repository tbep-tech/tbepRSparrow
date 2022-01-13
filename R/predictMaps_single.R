predictMaps_single<-function(mapType,mapLoopInput.list, p, plotdata,plotPageData,titleStr,subTitle,
                             legendPos,legendJus,usedColors,mapvarname,i){
  
  unPackList(lists = list(mapLoopInput.list = mapLoopInput.list),
             parentObj = list(NA))
  
  if (mapvarname!="MAPCOLORS"){
   mapvarname <- paste0("MAPCOLORS",k)  
  }
   
  
  if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
    plotdata$mapColor<-eval(parse(text = paste0("plotdata$",mapvarname)))
  }else if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){#plotly
    suppressWarnings(remove(list = c(add_plotlyVars)))
  }
 
  if (master_map_list[k]=="hydseq" | master_map_list[k]=="hydseq_new"){
    Mcolors<-Mcolors[[i]]
    break1[k][[1]]<-break1[k][[1]][[i]]
    
  }
  
  uniqueCols<-eval(parse(text = paste0("as.character(unique(plotPageData$",mapvarname,"))")))
  uniqueCols<-Mcolors[Mcolors %in% uniqueCols]
  break1[k][[1]]<-break1[k][[1]][which(Mcolors %in% uniqueCols)]
  strlegColor<-paste0("'", uniqueCols[1:length(break1[k][[1]])],"'='", uniqueCols[1:length(break1[k][[1]])],"'",collapse = ",")
  strlegColor<-paste0("c(",strlegColor,")")
  plotPageData$mapColor<-eval(parse(text=paste0("plotPageData$",mapvarname)))
  if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
    
    if (existGeoLines){
      if (mapType=="stream"){
        
        p<-p %+% geom_sf(data = plotdata, size = lineWidth, 
                         aes(colour = factor(mapColor,levels =  uniqueCols[1:length(break1[k][[1]])])),
                         show.legend = TRUE) +
          coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
          #scale_colour_manual(values = uniqueCols[1:length(break1[k][[1]])],
          scale_colour_manual(values = eval(parse(text = strlegColor)),
                              labels = break1[k][[1]],
                              name = titleStr,
                              drop=FALSE) +
          ggtitle(titleStr) +
          theme(plot.title = element_text(hjust = 0.5,size =predictionTitleSize, face = 'bold'),
                legend.position=legendPos,
                legend.justification = legendJus,
                legend.text = element_text(size = 24*predictionLegendSize),
                legend.title = element_text(size = 26*predictionLegendSize,face ='bold'),
                legend.background = element_rect(fill=predictionLegendBackground),
                legend.key.size = unit(predictionLegendSize, 'cm')) +
          guides(col=guide_legend(nrow=length(unique(plotPageData$mapColor)))) +
          ggtitle(subTitle) + theme(plot.title = element_text(hjust = 0.5))
        
      }else{#catchment
        p<-p %+% geom_sf(data = plotdata, size = lineWidth,
                         aes(fill = factor(mapColor,levels =  uniqueCols[1:length(break1[k][[1]])])),colour = NA,
                         show.legend = TRUE) +
          coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
          scale_fill_manual(values = eval(parse(text = strlegColor)),
                            labels = break1[k][[1]],
                            name = titleStr,
                            drop=FALSE) +
          ggtitle(titleStr) +
          theme(plot.title = element_text(hjust = 0.5,size =predictionTitleSize, face = 'bold'),
                legend.position=legendPos,
                legend.justification = legendJus,
                legend.text = element_text(size = 24*predictionLegendSize),
                legend.title = element_text(size = 26*predictionLegendSize,face ='bold'),
                legend.background = element_rect(fill=predictionLegendBackground),
                legend.key.size = unit(predictionLegendSize, 'cm')) +
          guides(fill = guide_legend(nrow=length(unique(plotPageData$mapColor)))) +
          ggtitle(subTitle) + theme(plot.title = element_text(hjust = 0.5))
      }#if catchemnt
      
    }else{#no geolines
      if (mapType=="stream"){
        p<-ggplot() +
          geom_sf(data = plotdata, size = lineWidth, 
                  aes(colour = factor(mapColor,levels =  uniqueCols[1:length(break1[k][[1]])])),
                  show.legend = TRUE) +
          coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
          scale_colour_manual(values = eval(parse(text = strlegColor)),
                              labels = break1[k][[1]],
                              name = titleStr,
                              drop=FALSE) +
          ggtitle(titleStr) +
          theme(plot.title = element_text(hjust = 0.5,size =predictionTitleSize, face = 'bold'),
                legend.position=legendPos,
                legend.justification = legendJus,
                legend.text = element_text(size = 24*predictionLegendSize),
                legend.title = element_text(size = 26*predictionLegendSize,face ='bold'),
                legend.background = element_rect(fill=predictionLegendBackground),
                legend.key.size = unit(predictionLegendSize, 'cm')) +
          guides(col=guide_legend(nrow=length(unique(plotPageData$mapColor)))) +
          ggtitle(subTitle) + theme(plot.title = element_text(hjust = 0.5))
      }else{#catchment
        p<-ggplot() +
          geom_sf(data = plotdata, #size = lineWidth,
                  aes(fill = factor(mapColor,levels =  uniqueCols[1:length(break1[k][[1]])])),colour = NA,
                  show.legend = TRUE) +
          coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
          scale_fill_manual(values = eval(parse(text = strlegColor)),
                            labels = break1[k][[1]],
                            name = titleStr,
                            drop=FALSE) +
          ggtitle(titleStr) +
          theme(plot.title = element_text(hjust = 0.5,size =predictionTitleSize, face = 'bold'),
                legend.position=legendPos,
                legend.justification = legendJus,
                legend.text = element_text(size = 24*predictionLegendSize),
                legend.title = element_text(size = 26*predictionLegendSize,face ='bold'),
                legend.background = element_rect(fill=predictionLegendBackground),
                legend.key.size = unit(predictionLegendSize, 'cm')) +
          guides(fill = guide_legend(nrow=length(unique(plotPageData$mapColor)))) +
          ggtitle(subTitle) + theme(plot.title = element_text(hjust = 0.5))
      }#if catchment  
      
    }
    
    #save legend
    
    # if (j==1){
    #   mylegend<-g_legend(p)
    # }
    # if (nrow(plotSub[!is.na(plotSub$year),])>1){
    #   p<-p %+% theme(legend.position = "none")
    # }
    
  }else if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){#plotly
    #for (c in unique(plotPageData$color)){
    for (c in uniqueCols){
      plotdata2<-plotdata
      plotdata2$mapColor<-eval(parse(text = paste0("plotdata2$",mapvarname)))
      plotdata2<-plotdata2[plotdata2$mapColor==c,]
      plotdata2$mapdataname<-eval(parse(text = paste0("plotdata2$",mapdataname)))
      
      lineText<-"~paste('</br> ',master_map_list[k],' :',
                   round(mapdataname,predictionClassRounding)"
      
      lineText<-addMarkerText(lineText,add_plotlyVars,plotdata2, plotdata2)$markerText
      
      if (mapType=="stream"){
        if (!c %in% usedColors & c %in% plotdata2$mapColor){
          if (nrow(plotdata2)!=0){
          usedColors<-c(usedColors,c)
          }
          p <- p %>% add_sf(data = plotdata2, mode = "lines", type = "scatter",
                            # color = I(c),
                            color = ~I(mapColor),
                            name = break1[k][[1]][uniqueCols==c],
                            line = list(width = lineWidth),
                            hoverinfo = 'text',
                            text = eval(parse(text = lineText)),
                            legendgroup=c, showlegend=TRUE)
        }else{
          p <- p %>% add_sf(data = plotdata2, mode = "lines", type = "scatter",
                            #color = I(c),
                            color=~I(mapColor),
                            name = break1[k][[1]][uniqueCols==c],
                            line = list(width = lineWidth),
                            hoverinfo = 'text',
                            text = eval(parse(text = lineText)),
                            legendgroup=c,showlegend=FALSE)
        }
      }else{#catchment
        if (!c %in% usedColors & c %in% plotdata2$mapColor){
          if (nrow(plotdata2)!=0){
          usedColors<-c(usedColors,c)
          }
          p <- p %>% add_sf(data = plotdata2[1,],
                            type = "scatter", mode = "lines",
                            # color = toRGB(c),
                            opacity = 1,fillcolor = toRGB(c),
                            line = list(color = toRGB(c),width = 0.8, opacity = 1),
                            name = break1[k][[1]][uniqueCols==c],
                            hoverinfo = 'text',
                            split = eval(parse(text = paste0("~",commonvar))),
                            hoveron = "fills",
                            legendgroup = c,
                            text = eval(parse(text = lineText)),
                            showlegend = TRUE)
          p <- p %>% add_sf(data = plotdata2[2:nrow(plotdata2),],
                            type = "scatter", mode = "lines",
                            # color = toRGB(c),
                            opacity = 1,fillcolor = toRGB(c),
                            line = list(color = toRGB(c),width = 0.8, opacity = 1),
                            hoverinfo = 'text',
                            split = eval(parse(text = paste0("~",commonvar))),
                            hoveron = "fills",
                            legendgroup = c,
                            text = eval(parse(text = lineText)),
                            showlegend = FALSE)
        }else{
          
          p <- p %>% add_sf(data = plotdata2, mode = "lines",
                            #p <- p %>% add_sf(data = plotdata2[2:nrow(plotdata2),],
                            type = "scatter", mode = "lines",
                            # color = toRGB(c),
                            opacity = 1,fillcolor = toRGB(c),
                            line = list(color = toRGB(c),width = 0.8, opacity = 1),
                            hoverinfo = 'text',
                            split = eval(parse(text = paste0("~",commonvar))),
                            hoveron = "fills",
                            legendgroup = c,
                            text = eval(parse(text = lineText)),
                            showlegend = FALSE)
        }
      }
      
      
      
      
      
    }
    
  }else{#leaflet
    if (mapType=="stream"){
    plotdata$mapColor<-eval(parse(text = paste0("plotdata$",mapvarname)))
    plotdata$mapdataname<-eval(parse(text = paste0("plotdata$",mapdataname)))
    lineText<-"~paste('</br> ',master_map_list[k],' :',
                   round(mapdataname,predictionClassRounding)"
    
    lineText<-addMarkerText(lineText,add_plotlyVars,plotdata, plotdata)$markerText
    
    lineText<-gsub("~","",lineText)
    lineTextHTML<-paste0("~lapply(",lineText,",HTML)")
    
    plotdata<-st_transform(plotdata, crs = 4326)
    plotdata<-st_zm(plotdata, drop = T, what = "ZM")
    p <- mapview(plotdata, fill = F, homebutton = F, popup = NULL, legend = F, viewer.suppress = F) %>% 
      .@map %>% 
      clearMarkers() %>% 
      clearShapes() %>% 
      addPolylines(
        data = plotdata, 
        opacity = 1,
        weight = lineWidth,
        color = ~col2hex(mapColor),
        label = eval(parse(text = lineTextHTML))
      ) %>% 
      addLegend("bottomleft", labels = break1[k][[1]], colors = col2hex(uniqueCols),
                title = titleStr, opacity = 1)
    }else{#catchment
      plotdata$mapColor<-eval(parse(text = paste0("plotdata$",mapvarname)))
      plotdata$mapdataname<-eval(parse(text = paste0("plotdata$",mapdataname)))
      
      lineText<-"~paste('</br> ',master_map_list[k],' :',
                   round(mapdataname,predictionClassRounding)"
      
      lineText<-addMarkerText(lineText,add_plotlyVars,plotdata, plotdata)$markerText
      
      lineText<-gsub("~","",lineText)
      lineTextHTML<-paste0("~lapply(",lineText,",HTML)")
      
      plotdata<-st_transform(plotdata, crs = 4326)
      plotdata<-st_zm(plotdata, drop = T, what = "ZM")
      p <- mapview(plotdata, fill = F, homebutton = F, popup = NULL, legend = F, viewer.suppress = F) %>% 
        .@map %>% 
        clearMarkers() %>% 
        clearShapes() %>% 
        addPolygons(
          data = plotdata, 
          color = 'grey', 
          weight = 0, 
          stroke = FALSE,
          fillColor = ~col2hex(mapColor),
          fillOpacit = 0.9,
          label = eval(parse(text = lineTextHTML))
        ) %>% 
        addLegend("bottomleft", labels = break1[k][[1]], colors = col2hex(uniqueCols),
                  title = titleStr, opacity = 1)
      
    }
  }
  
  p.list<-named.list(p,usedColors)
  return(p.list)
}