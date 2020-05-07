#'@title diagnosticMaps
#'@description Creates diagnostic maps of residuals and site attributes
#'Uses subroutines: errorOccurred.
#'@param mapColumn character string indicating column of data to be mapped
#'@param mapdata input data.frame with lat, long, and column to be mapped 
#'@param GeoLines Optional geospatial shape file for overlay of lines on stream/catchment maps
#'@param lat_limit User specified geographic units minimum/maximum latitude limits for mapping of Residuals and prediction maps
#'@param lon_limit User specified geographic units minimum/maximum longitude limits for mapping of Residuals and prediction maps
#'@param residual_map_breakpoints User specified breakpoints for mapping of residuals. Numeric vector with length of 7 centered on 0.
#'@param site_mapPointScale User specified scaling factor for points
#'@param map.list character string indicating whether over/under predictions ("threshold") are to be mapped or all predictions ("all"). Value c("threshold","all")
#'@param strTitle character string for plot title 
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit,lon_limit,master_map_list,
#'lineShapeName,lineWaterid,polyShapeName,ployWaterid,LineShapeGeo, LineShapeGeo,CRStext,convertShapeToBinary.list,
#'map_siteAttributes.list,residual_map_breakpoints,site_mapPointScale, if_verify_demtarea_maps 
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`


diagnosticMaps<-function(mapColumn,mapdata,GeoLines,lat_limit,lon_limit,
                         map.list,strTitle,mapping.input.list,batch_mode,ErrorOccured){
  
  if (ErrorOccured=="no"){
    tryIt<-try({

      # Setup variable lists 
      # create global variable from list names (mapping.input.list)
      for(i in 1:length(mapping.input.list)){
        tempobj=mapping.input.list[[i]]
        eval(parse(text=paste(names(mapping.input.list)[[i]],"= tempobj")))
      }
    
        
  #get data
      mapdata <- mapdata
  
  #set up according to maptype
  if (regexpr("Resid",mapColumn,ignore.case = TRUE)>0 ){
    #set breakpoints
    if (is.na(residual_map_breakpoints) | length(residual_map_breakpoints)!=7){
      cls <- c(-2.5,-0.75,-0.25,0,0.25,0.75,2.5)  # Residual breakpoints
    }else{
      cls<-residual_map_breakpoints
    }

    #set threshold and data column
    threshold<-0
    
  }else{# Ratio breakpoints
    if (is.na(ratio_map_breakpoints) | length(ratio_map_breakpoints)!=7){
      cls <-  c(0.3,0.5,0.8,1,1.25,2,3.3)    # Residual breakpoints
    }else{
      cls<-ratio_map_breakpoints
    }
      
    
    #set threshold and data column
    threshold<-1
    
  }#end setup mapType    

    
    #point size and color  
    sze <- residualPointSize_breakpoints*residualPointSize_factor  # Symbol sizes
    color <- residualColors  
    cbckgrd <- residualMapBackground
    
    # Symbol types:
    # 2 = upward triangle
    # 6 = downward triangle
    # 1 = open circle
    pnch <- residualPointStyle
    
    if ("threshold" %in% map.list){   
      par(mfrow=c(1,1), pch=16)    # 1 plots on one page
      #subset data
      above <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,"<",threshold,"),]",sep="")))  # over predictions (neg residuals)
      below <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,">",threshold,"),]",sep="")))
      nabove <- eval(parse(text=paste("length(above$",mapColumn,")",sep="")))
      nbelow <- eval(parse(text=paste("length(below$",mapColumn,")",sep="")))
      
    #for below threshold
    plot(GeoLines,lwd=0.1,xlim=lon_limit,ylim=lat_limit,col=1,bg=cbckgrd)
    title(bquote(paste(.(strTitle)," - Over Predictions - n=",.(nabove))),cex.main=residualTitleSize)
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,"<= cls[1]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
    
    strLegend<-paste("< ",cls[1],sep="")
    
    for (k in 1:3) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn,"<= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1])
      
      strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      strLegend<-c(strLegend,strlegend)
    }
    
    legend("bottomleft",strLegend,
           bg=cbckgrd, bty="o",pch = residualPointStyle[1:4], pt.cex = residualPointSize_breakpoints[1:4]*residualPointSize_factor, col=color[1:4]
           ,cex=residualLegendSize)
    
   
    #for above threshold
    plot(GeoLines,lwd=0.1,xlim=lon_limit,ylim=lat_limit,col=1,bg=cbckgrd)
    title(bquote(paste(.(strTitle)," - Under Predictions - n=",.(nbelow))),cex.main=residualTitleSize)
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[7]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8])  
    
    strLegend<-vector('character')
    
    for (k in 4:7) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
      
      if (k!=7){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste("> ",cls[k],sep="")
      }
      strLegend<-c(strLegend,strlegend)
    }
    
    legend("bottomleft",strLegend,
           bg=cbckgrd, bty="o",pch = residualPointStyle[5:8], pt.cex = residualPointSize_breakpoints[5:8]*residualPointSize_factor, 
           col=color[5:8],cex=residualLegendSize)
    }#end if threshold map    
    
    if ("all" %in% map.list){
    #for all cls
    par(mfrow=c(1,1), pch=16)    # 1 plots on one page
    
    plot(GeoLines,lwd=0.1,xlim=lon_limit,ylim=lat_limit,col=1,bg=cbckgrd)
    title(strTitle,cex.main=residualTitleSize)
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," <= cls[1]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
    
    strLegend<-paste("< ",cls[1],sep="")
    
    for (k in 1:7) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
      
      if (k!=7){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste("> ",cls[k],sep="")
      }
      strLegend<-c(strLegend,strlegend)
    }
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[7]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8])  
    
    
    legend("bottomleft",strLegend,
           bg=cbckgrd, bty="o",pch = residualPointStyle, 
           pt.cex = sze, col=color,cex=residualLegendSize)
  
    }#end if all  
    
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("diagnosticMaps.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function