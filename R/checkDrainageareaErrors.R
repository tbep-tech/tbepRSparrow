#'@title checkDrainageareaErrors
#'@description Executes drainage area checks for newly computed areas, based on the 
#'            if_verify_demtarea control setting (section 2 of the control script). If any differences are found 
#'            between the user's original data for Total Drainage Area vs. Total Drainage Area calculated by 
#'            RSPARROW, a plot of user's original data for Total Drainage Area vs. Total Drainage Area calculated 
#'            by RSPARROW is output. For the control setting if_verify_demtarea_maps<-"yes", maps are output 
#'            of `demtarea` and `hydseq` for unmatched areas as a ratio of RSPARROW calculated:original. A CSV 
#'            file is output of all differences found to ~/estimate/(run_id)_diagnostic_darea_mismatches.csv. \\cr \\cr
#'Executed By: verifyDemtarea.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param DAreaFailCheckObj data.frame of all rows of subdata in which the user's original data 
#'       for Total Drainage Area vs. Total Drainage Area calculated by RSPARROW differ
#'@param data1 input data (data1)
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



checkDrainageareaErrors <- function(file.output.list,mapping.input.list,
                                    #sub1.plot,
                                    DAreaFailCheckObj,data1,
                                    batch_mode) {
  
  # Setup variable lists 
  # create global variable from list names (mapping.input.list)
  unPackList(lists = list(file.output.list = file.output.list,
                          mapping.input.list = mapping.input.list),
             parentObj = list(NA, NA)) 
  
  filename1 <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_darea_mismatches.html")
  if (length(na.omit(DAreaFailCheckObj$demtarea))!=0){
    
    if (if_verify_demtarea_maps=="yes"){
      #get geoLines
      existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
      
      #get lineShape
      existlineShape<-checkBinaryMaps(lineShapeName,path_gis,batch_mode)
      
      commonvar <- lineWaterid
      
      ##############################################################
      # Loop through variable list
      
      #if (!is.na(LineShapeGeo) & !is.na(lineShapeName)) {  # map if shape files available
      if (existGeoLines & existlineShape){# map if shape files available
        
        map.vars.list <- c("demtarea","hydseq","hydseq_new","AreaRatio_NewOld")
        title_name <- c("Pre-calculated DEMTAREA","HYDSEQ","new HYDSEQ for unmatched areas","AreaRatio_New:Old")
        
        
          #path_checkDrainageareaErrorsChild<-file_path_as_absolute(paste0(path_master,"checkDrainageareaErrorsChild.Rmd"))
          
          reportPath<-paste0(path_master,"checkDrainageareaErrors.Rmd")
            
          filename1<-gsub("pdf","html",filename1)


            
            rmarkdown::render(paste0(path_master,"checkDrainageareaErrors.Rmd"),
              params = list(
                file.output.list = file.output.list,
                mapping.input.list = mapping.input.list,
                DAreaFailCheckObj = DAreaFailCheckObj, 
                data1 = data1,
                existGeoLines = existGeoLines,
                commonvar = commonvar,
                map.vars.list = map.vars.list,
                GeoLines = GeoLines,
                lineShape = lineShape,
                title_name = title_name,
                filename = filename1
              ),
              output_file = filename1, quiet = TRUE
            )
            shell.exec(filename1)
           ##################################### 
            mapunits.list<-data_names[data_names$sparrowNames %in% map.vars.list,]
            mapunits.list<-mapunits.list[match(mapunits.list$sparrowNames,map.vars.list),]$varunits
            break2<-list()
            for (k in 1:length(map.vars.list)){
              commonvar<-"tempID"
              
              
              
              aggFuncs<-c("mean","median","min","max")
              if (map_years=="all" | map_years %in% aggFuncs){
                map_years<-unique(data1$year)
              }
              if (map_seasons=="all" | map_seasons %in% aggFuncs){
                map_seasons<-unique(data1$season)
              }
            
            prepReturns.list<-checkDrainageareaMapPrep(file.output.list,mapping.input.list,
                                                       DAreaFailCheckObj,data1, 
                                                       existGeoLines, commonvar, map.vars.list, k)
            
            makePlot<-!is.na(prepReturns.list)
            if (makePlot){
              unPackList(lists = list(prepReturns.list = prepReturns.list),
                         parentObj = list(NA)) 
           
            if (!k %in% c(2,3)){
              break2<-list()
            break2[k][[1]]<-break1
            break1<-break2
            }
            # if (!is.na(add_plotlyVars[1])){
              assign("dmap",dmap,envir = .GlobalEnv)
              assign("data1",data1,envir = .GlobalEnv)
              
              assign("break1",break1,envir = .GlobalEnv)
              assign("Mcol",Mcol,envir = .GlobalEnv)
             
              #data1Merge<-merge(dmap,data1,by.x = commonvar, by.y = "waterid")
              #names(data1Merge)[names(data1Merge)==commonvar]<-"waterid"
              #data1Merge<-data1Merge[,names(data1Merge) %in% names(data1)]
              
              if ((enable_plotlyMaps!="no" & enable_plotlyMaps!="static") | !is.na(add_plotlyVars[1])){
                
                if ((!is.na(map_seasons) & !map_seasons %in% aggFuncs) & (!is.na(map_years) & !map_years %in% aggFuncs)){
                  data1Merge<-merge(dmap,data1,by.x = commonvar, by.y = "waterid")
                  names(data1Merge)[names(data1Merge)==commonvar]<-"waterid_for_RSPARROW_mapping"
                  data1Merge<-data1Merge[,names(data1Merge) %in% names(data1)]
                  
                  if (is.na(map_years) & is.na(map_seasons)){
                    dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon")), dmap, data1Merge)$mapData
                  }else if (is.na(map_seasons)){
                    dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon","year","mapping_waterid")), dmap, data1Merge)$mapData
                  }else if (is.na(map_years)){
                    dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon","season","mapping_waterid")), dmap, data1Merge)$mapData
                  }else{
                    dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon","year","season","mapping_waterid")), dmap, data1Merge)$mapData
                  }
                  
                
                  
                }else{
                  if (is.na(map_years) & is.na(map_seasons)){
                    NAMES<-unique(c(names(dmapfinal),add_plotlyVars,"lat","lon"))
                  }else if (is.na(map_seasons)){
                    NAMES<-unique(c(names(dmapfinal),add_plotlyVars,"lat","lon","year","mapping_waterid"))
                  }else if (is.na(map_years)){
                    NAMES<-unique(c(names(dmapfinal),add_plotlyVars,"lat","lon","season","mapping_waterid"))
                  }else{
                    NAMES<-unique(c(names(dmapfinal),add_plotlyVars,"lat","lon","year","season","mapping_waterid"))
                  }
                  dmapfinal<-merge(dmap,data1,by.x = names(dmap)[names(dmap) %in% names(data1)],
                                   by.y = names(dmap)[names(dmap) %in% names(data1)])
                  dmapfinal<-dmapfinal[,names(dmapfinal) %in% NAMES]
                  
                }
                
              }
              
              # dmap<-addMarkerText("",c(add_plotlyVars, "lat","lon"), dmap, data1Merge)$mapData
              # dmapfinal<-dmap
            #}
            
              commonvar <- lineWaterid
              names(dmapfinal)[1]<-commonvar
            
            reportPath<-paste0(path_master,"outputMaps.Rmd")
            
            #create plot sequence
            plots<-setupDynamicMaps(dmapfinal,map_years,map_seasons,mapPageGroupBy,mapsPerPage, Rshiny=FALSE, enable_plotlyMaps)

            # if ((input$batch=="Batch" & Rshiny) | !Rshiny){
            # }else{
            #   mapdataname <- paste0("vvar",k) 
            # }
            input<-list(variable="",scLoadCheck="",batch="",scYieldCheck="",domain="",selectReaches="",sourcesCheck="",factors="")
            
            assign("dmapfinal",dmapfinal,envir=.GlobalEnv)
            
            mapLoopInput.list<- list(
              file.output.list = file.output.list,
              GeoLines = GeoLines,
              plotShape = lineShape,
              dmapfinal = dmapfinal,
              plots = plots,
              k = k,
              existGeoLines = existGeoLines,
              Rshiny = FALSE,
              input = input,
              predictionTitleSize = predictionTitleSize,
              scenario_name = NA,
              scenario_map_list = NA,
              master_map_list = map.vars.list,
              predictionLegendSize = predictionLegendSize,
              mapunits.list = mapunits.list,
              predictionLegendBackground = predictionLegendBackground,
              break1 = break1,
              Mcolors = Mcol,
              enable_plotlyMaps = enable_plotlyMaps,
              output_map_type = output_map_type,
              lineWidth = lineWidth,
              lon_limit = lon_limit,
              lat_limit = lat_limit,
              nlty = nlty,
              nlwd = nlwd,
              CRStext = mapping.input.list$CRStext,
              mapdataname = "VVAR",
              predictionMapColors = Mcol,
              add_plotlyVars = add_plotlyVars,
              mapScenarios = FALSE,
              predictionMapBackground = predictionMapBackground,
              LineShapeGeo = LineShapeGeo,
              mapvarname = "MAPCOLORS",
              predictionClassRounding = predictionClassRounding,
              commonvar = lineWaterid,
              map_years = map_years,
              map_seasons = map_seasons,
              mapsPerPage = mapsPerPage,
              mapPageGroupBy = mapPageGroupBy,
              aggFuncs = aggFuncs
            )
            
            
            # if ((input$batch=="Batch" & Rshiny) | !Rshiny){
              
            if (!existGeoLines){GeoLines<-NA}
            if (!dir.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnostic_darea_mismatches_maps"))){
              dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnostic_darea_mismatches_maps"))
            }
              filename <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnostic_darea_mismatches_maps",
                                 .Platform$file.sep,map.vars.list[k],".pdf")
              htmlFile<-gsub("pdf","html",filename)
             
              #path_predictMapsChild<-file_path_as_absolute(paste0(path_master,"predictMapsChild.Rmd"))
              rmdTitle<-file.output.list$run_id
              path_outputMapsChild<-file_path_as_absolute(paste0(path_master,"outputMapsChild.Rmd"))
              
              rmarkdown::render(paste0(path_master,"outputMaps.Rmd"),
                                params = list(
                                  rmdTitle = rmdTitle,
                                  mapType = "stream",
                                  mapLoopInput.list = mapLoopInput.list,
                                  path_outputMapsChild = path_outputMapsChild
                                ),
                                output_file = htmlFile, quiet = TRUE
              )
            }#if makeplot
            }#for k
           ############################################ 
        #}#else plotly
      } # execute if shape files exist
    }
    
    
    #popup pdf of plot
   
  }#end if all missing original demtarea
  
  # Output mis-matched reach data
  waterid <- DAreaFailCheckObj$waterid
  fnode_pre <- DAreaFailCheckObj$fnode
  tnode_pre <- DAreaFailCheckObj$tnode
  frac_pre <- DAreaFailCheckObj$frac
  demtarea_pre <- DAreaFailCheckObj$demtarea
  demtarea_post <- DAreaFailCheckObj$demtarea_new
  hydseq_new <- DAreaFailCheckObj$hydseq_new
  headflag_new <- DAreaFailCheckObj$headflag_new
  headflag_check <- DAreaFailCheckObj$Headflag_NewOld
  AreaRatio_NewOld <- DAreaFailCheckObj$AreaRatio_NewOld
  
  origWaterid<-data1[,which(names(data1) %in% c("waterid","waterid_for_RSPARROW_mapping"))]
  origWaterid<-origWaterid[which(origWaterid$waterid %in% waterid),]
  origWaterid<-origWaterid[order(match(origWaterid$waterid,waterid)),]
  origWaterid<-origWaterid$waterid_for_RSPARROW_mapping
  
  pout <- data.frame(waterid,origWaterid,fnode_pre,tnode_pre,frac_pre,demtarea_pre,demtarea_post,hydseq_new,
                     AreaRatio_NewOld,headflag_new,headflag_check)
  fileout <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_darea_mismatches.csv")
  write.table(pout,file=fileout,row.names=F,append=F,quote=F,
              dec=csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE)
  
  
}#end function
