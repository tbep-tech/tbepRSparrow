#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))

  #get paths
  load(gsub("interactiveBatchRun.R","interactiveBatch.RData",res))


  #load RSPARROW
  devtools::load_all(path_main,recompile = FALSE)
  
  #get batch plot data
  load(batchFilename)  
  

  
  #run interactive batch plot

  
 # if (inputShiny$mapType %in% c("Stream","Catchment")){
    
    
    #compile metrics
#    allMetrics<-c(inputShiny$loadCheck,inputShiny$yieldCheck,inputShiny$uncertaintyCheck,inputShiny$varCheck)
    
 # }else if (inputShiny$mapType=="Source Reduction Scenarios"){
    #compile metrics
  #  allMetrics<-c(inputShiny$scLoadCheck,inputShiny$scYieldCheck)

   #   output_map_type<-tolower(as.character(inputShiny$outCheck))
   
  #}
  
  #compile metrics
  allMetrics<-as.character(unlist(inputShiny[which(regexpr("Check",names(inputShiny))>0 & names(inputShiny)!="outCheck")]))
  
  if (inputShiny$mapType=="Source Reduction Scenarios"){
    output_map_type<-tolower(as.character(inputShiny$outCheck))
    }
  
  
  if (inputShiny$mapType=="Stream" | inputShiny$mapType=="Catchment"){
   mapScenarios<-FALSE
     predictMaps(inputShiny,allMetrics,output_map_type, TRUE,
                path_results,file_sum,path_gis,
                data_names,mapping.input.list,
                subdata,
                #scenarios
                mapScenarios,
                scenario_map_list,
                predictScenarios.list,
                scenario_name,
                batch_mode,ErrorOccured)
     

    
    
  }else if (inputShiny$mapType=="Site Attributes"){
    
    for (a in inputShiny$dataCheck){
      filename<- paste(path_results,"/maps/Interactive/SiteAttributes/",file_sum,"_SiteAttributes_",a,".pdf",sep="")
      pdf(filename)

      mapSiteAttributes(#Rshiny
                        inputShiny,a, path_gis, sitedata, LineShapeGeo,data_names,TRUE,
                        #regular
                        mapColumn,mapdata,GeoLines,mapping.input.list,
                        strTitle,unitAttr,batch_mode,ErrorOccured)
      
      dev.off()
      if (a==inputShiny$dataCheck[length(inputShiny$dataCheck)]){
        shell.exec(filename)  
      }
    }#for each attr
    
    #output siteAttr shapefile
    
    if (inputShiny$shapeFile=="yes"){
      xlat<-Mdiagnostics.list$xlat
      xlon<-Mdiagnostics.list$xlon
      map_siteAttributes.list<-as.character(inputShiny$attrCheck)
      CRStext<-mapping.input.list$CRStext
      siteAttrshape<-data.frame(xlat,xlon)
      for (s in 1:length(map_siteAttributes.list)){
        if (length(names(sitedata)[which(names(sitedata)==map_siteAttributes.list[s])])!=0){
          siteAttr<-eval(parse(text= paste("data.frame(",map_siteAttributes.list[s],"=sitedata$",map_siteAttributes.list[s],")",sep="")))
          siteAttrshape<-data.frame(siteAttrshape,siteAttr)
          names(siteAttrshape)[length(siteAttrshape)]<-map_siteAttributes.list[s]
        }
      }
      
   
      
      siteAttrshape<-sp::SpatialPointsDataFrame(siteAttrshape[,c("xlon","xlat")],siteAttrshape[,which(!names(siteAttrshape) %in% c("xlat","xlon"))],proj4string=sp::CRS(CRStext))
      
      if (!dir.exists(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/",sep=""))){
        dir.create(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
      }
      if (!dir.exists(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/siteAttributes/",sep=""))){
        dir.create(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/siteAttributes/",sep=""),showWarnings = FALSE)
      }
      
      maptools::writeSpatialShape(siteAttrshape,paste(path_results,"maps/Interactive/ESRI_ShapeFiles/siteAttributes/siteAttrshape",sep=""))
      cat(rgdal::showWKT(sp::proj4string(siteAttrshape)),file=paste(path_results,"/maps/Interactive/ESRI_ShapeFiles/siteAttributes/siteAttrshape.prj",sep="")) 
    }
    
  }else if (inputShiny$mapType=="Source Reduction Scenarios"){
    

    predictScenarios(#Rshiny
      inputShiny,"pload_total", output_map_type,TRUE,
      #regular
      predict.list,if_predict_scenarios,scenario_sources,scenario_all_factors,
      data_names,estimate.list,ConcFactor, if_predict,
      #bootcorrection,
      DataMatrix.list,SelParmValues,
      reach_decay_specification,reservoir_decay_specification,subdata,
      #predictStreamMapScenarios
      path_results,file_sum,path_gis,
      #scenarios out
      add_vars,csv_decimalSeparator,csv_columnSeparator,
      mapping.input.list,
      batch_mode,ErrorOccured)
    

    
  }
