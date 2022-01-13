diagnosticResidMapExec<-function(validation,existGeoLines,residmapTypes,mapColumn,strTitle,dynamic,
                                 sitedata, Resids,pResids,ratio.obs.pred,pratio.obs.pred,standardResids,
                                 path_diagnosticDiagMapChild,path_outputMapsChild,path_outputMaps,
                                 map_years,map_seasons,mapPageGroupBy,mapsPerPage,
                                 enable_plotlyMaps,add_plotlyVars,path_gis,LineShapeGeo,GeoLines,
                                 data_names,mapping.input.list,batch_mode){

# Setup GEOLINES basemap, if available
if (!validation){ 
  #8.8.17 if(!is.na(LineShapeGeo)) {
  if(existGeoLines) { 
    
    # residmapTypes<-c("threshold-above","threshold-below","all")
    # mapColumn<-"Resids"
    # 
    # #mapdata<-mapdata
    # strTitle<-"Model Estimation Log Residuals"
    
    
    if (!dynamic){
      mapdata <- data.frame(lat=sitedata$lat,lon=sitedata$lon,Resids,ratio.obs.pred,pResids,pratio.obs.pred,standardResids) 
      rmd <- sapply(
        1:(length(residmapTypes)),
        function(n) {
          
          knit_expand(path_diagnosticDiagMapChild, n = n)
        }
      )
      rmd <- paste(rmd, collapse = "\n")
      return(rmd)
    }else{#dynamic
      eval(parse(text=paste0("residData<-",mapColumn)))
      MAPID <- eval(parse(text=paste0("sitedata$","waterid_for_RSPARROW_mapping") )) 
      commonvar<-"tempID"
      mapType<-"resid"
      aggFuncs<-c("mean","median","min","max")
      
      if (map_years=="all" | map_years %in% aggFuncs){
        map_years<-unique(sitedata$year)
      }
      if (map_seasons=="all" | map_seasons %in% aggFuncs){
        map_seasons<-unique(sitedata$season)
      }
      
      if (length(map_years)!=0){
        map_years<-as.numeric(map_years)
      }else if (length(map_years)==0){
        map_years<-NA
      }
      if (length(map_seasons)==0){
        map_seasons<-NA
      }
      
      for (n in residmapTypes){
        
        input<-list(var=NA, sizeBatch=NA,size=NA)
        map.list<-n 
        
        agg_map.list<-aggDynamicMapdata(map_years,map_seasons,
                                        enable_plotlyMaps,
                                        add_plotlyVars,
                                        aggFuncs,vvar = residData,MAPID,commonvar,subdata = sitedata)
        unPackList(lists = list(agg_map.list = agg_map.list),
                   parentObj = list(NA)) 
        
        #prep aggdata for plots
        mapdata<-merge(uniqueSubdata,mapdata, 
                       by=names(mapdata)[names(mapdata) %in% names(uniqueSubdata)])
        names(mapdata)[names(mapdata)=="vvar"]<-mapColumn
        if (map_years %in% aggFuncs | map_seasons %in% aggFuncs){
          names(mapdata)[names(mapdata)==commonvar]<-"mapping_waterid"
          add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","mapping_waterid",add_plotlyVars))
        }else{
          names(mapdata)[names(mapdata)==commonvar]<-"waterid_for_RSPARROW_mapping" 
          add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","waterid_for_RSPARROW_mapping",add_plotlyVars))
        }
        
        #mapdata$Resids<-Resids
        eval(parse(text=paste0("mapdata$",mapColumn,"<-",mapColumn)))
        
        
        plots<-setupDynamicMaps(mapdata,map_years,map_seasons,
                                mapPageGroupBy,mapsPerPage, Rshiny=FALSE, 
                                enable_plotlyMaps)
        mapLoopInput.list<-list(plots = plots,
                                map.list=map.list,
                                input = input,
                                path_gis = path_gis, 
                                sitedata = sitedata, 
                                LineShapeGeo = LineShapeGeo,
                                data_names = data_names,
                                Rshiny = FALSE,
                                #regular
                                mapColumn = mapColumn,
                                dmapfinal = mapdata,
                                GeoLines = GeoLines,
                                mapping.input.list = mapping.input.list,
                                strTitle = strTitle,
                                batch_mode = batch_mode)
        
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep))
        }
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep))
        }
        filename<- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep,paste0(gsub(" ","",strTitle),"_",n),".html")
        rmdTitle<-paste0(gsub(" ","",strTitle),"_",n)
        
        rmarkdown::render(path_outputMaps,
                          params = list(
                            rmdTitle = rmdTitle,
                            mapType = "resid",
                            mapLoopInput.list = mapLoopInput.list,
                            path_outputMapsChild = path_outputMapsChild
                          ),
                          envir = new.env(),
                          output_file = filename, quiet = TRUE
        )           
        
      }#resid map types 
      
    }#dynamic
  }#existGeolines
}#!validation
}#end func