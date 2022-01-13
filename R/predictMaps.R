#'@title predictMaps
#'@description executes prediction and source reduction scenario stream and catchment mapping \\cr \\cr
#'Executed By: \\itemize\{\\item batchMaps.R
#'             \\item interactiveBatchRun.R
#'             \\item goShinyPlot.R
#'             \\item predictScenarios.R\} \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item mapBreaks.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param allMetrics character string of all load, yield, uncertainty, and data dictionary 
#'       variables to map in shiny batch mode
#'@param output_map_type character string control setting to identify type of map(s) to output 
#'       to PDF file from "stream","catchment", or "both"
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param subdata data.frame input data (subdata)
#'@param mapScenarios TRUE/FALSE indicating whether source change scenario mapping is being run
#'@param scenario_map_list character vector of load and yield metrics to map in the source 
#'       change scenario
#'@param predictScenarios.list an archive with key scenario control settings and the load and 
#'                             yield prediction variables that are output from the execution of 
#'                             a source-change scenario evaluation. For more details see 
#'                             documentation Section 5.5.9
#'@param scenarioFlag binary vector indicating whether a reach is included in the source 
#'       reduction scenario
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



predictMaps<-function(#Rshiny
  input,allMetrics,output_map_type, Rshiny, 
  #regular
  file.output.list,
  data_names,mapping.input.list,
  subdata,
  #scenarios
  mapScenarios,
  scenario_map_list,
  predictScenarios.list,
  scenarioFlag,
  batch_mode) {
  

  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  # obtain uncertainties, if available
  objfile <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_BootUncertainties")
  if(file.exists(objfile)) {
    load(objfile)
    map_uncertainties <- c("se_pload_total","ci_pload_total")
  } else {
    map_uncertainties <- NA 
    BootUncertainties <- NA
  }
  
  testList<-character(0)
  if (!mapScenarios){
    #test if mapped variable not prediction or scenario
    if (Rshiny){ #shiny
      if (input$batch=="Interactive"){
        master_map_list<-c(trimws(gsub("-","",input$var)))
        
      }else{
        master_map_list<-c(trimws(gsub("-","",allMetrics)))
      }
      scenario_name<-character(0)
    }else{#not shiny
      master_map_list<-mapping.input.list$master_map_list
    }
    testList<-names(subdata)[which(names(subdata) %in% master_map_list)]
    noMaplist<-character(0)
    #only try to map variables with same length as subdata
    if (length(testList)!=0){
      for (l in testList){
        testvar<-subdata[,which(names(subdata)==l)]
        testvar<-na.omit(testvar)
        if (length(testvar)!=nrow(subdata)){
          lengthNA<-nrow(subdata)-length(testvar)
          if (lengthNA==nrow(subdata)){#all missing
            testList<-testList[which(testList!=l)]
            noMaplist<-c(noMaplist,l)
            message(paste0("\n \nWARNING : ALL MISSING VALUES FOUND IN ", l, " MAPPING NOT COMPLETED."))
            if (batch_mode=="yes"){
              cat(' \nWARNING : ALL MISSING VALUES FOUND IN ', l, ' MAPPING NOT COMPLETED.',sep='')
            }
          }
          
          
          message(paste0("\n \nWARNING : MISSING VALUES FOUND IN ", l, " MAPPING MAY NOT BE COMPLETE."))
          message(paste0(lengthNA," MISSING VALUES FOUND AND REPLACED WITH ZEROS \n \n"))
          if (batch_mode=="yes"){
            cat(' \nWARNING : MISSING VALUES FOUND IN ', l, ' MAPPING MAY NOT BE COMPLETE.',sep='')
            cat(lengthNA," MISSING VALUES FOUND AND REPLACED WITH ZEROS\n ",sep="")
          }
        }#remove variable from mapping list
        
        #test numeric
        testvar<-subdata[,which(names(subdata)==l)]
        testNum<-class(testvar)
        if (testNum!="numeric" & any(unique(as.numeric(testvar)-testvar)!=0)){
          testList<-testList[which(testList!=l)]
          noMaplist<-c(noMaplist,l)
          message(paste0("\n \nWARNING : MAPPING VARIABLE ", l, " NOT NUMERIC MAPPING NOT COMPLETED."))
          if (batch_mode=="yes"){
            cat(' \nWARNING : MAPPING VARIABLE ', l, ' NOT NUMERIC MAPPING NOT COMPLETED.',sep='')
          }
        }
        
      }#for each non prediction variable
    }#if non prediction variables exist
    
  }#if map scenarios FALSE
  
  if ((file.exists(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list")) 
       & !mapScenarios) | 
      mapScenarios |
      (length(testList)>0 & !mapScenarios)){
    if (!mapScenarios & file.exists(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list"))){
      if (!exists("predict.list")){
        load(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list"))
      }
    }
    
    
    # Setup variable lists 
    # create global variable from list names (mapping.input.list)
    output_map_typeArg<-output_map_type
    unPackList(lists = list(mapping.input.list = mapping.input.list),
               parentObj = list(NA))
    output_map_type<-output_map_typeArg
    
    if (!mapScenarios & exists("predict.list")){
      # create global variable from list names (predict.list)
      unPackList(lists = list(predict.list = predict.list),
                 parentObj = list(NA))
      
    }else if (mapScenarios){
      # create global variable from list names (predict.list)
      unPackList(lists = list(predictScenarios.list = predictScenarios.list),
                 parentObj = list(NA))
      
    }else if (!exists("predict.list")){
      master_map_list<-master_map_list[which(master_map_list %in% names(subdata))]
    }
    
    # required names
    datalstreq <- data_names$sparrowNames
    datalstunits <- data_names$varunits
    
    # transfer required variables to global environment from SUBDATA
    unPackList(lists = list(datalstreq = datalstreq),
               parentObj = list(subdata = subdata))
    
    if (Rshiny){ #shiny
      if (input$batch=="Interactive"){
        master_map_list<-c(trimws(gsub("-","",input$var)))
        
      }else{
        master_map_list<-c(trimws(gsub("-","",allMetrics)))
        
      }
      
      # get cosmetic mapping variables
      predictionTitleSize<-as.numeric(input$predictionTitleSize)
      predictionLegendSize<-as.numeric(input$predictionLegendSize)
      predictionLegendBackground<-gsub("\"","",gsub("'","",input$predictionLegendBackground))  
      predictionClassRounding<-as.numeric(input$predictionClassRounding)
      predictionMapBackground<-gsub("\"","",gsub("'","",input$predictionMapBackground))
      lineWidth<-as.numeric(input$lineWidth)
      if (!mapScenarios){
        predictionMapColors<-eval(parse(text=input$predictionMapColors))
      }else{
        scenarioMapColors<-eval(parse(text=input$scenarioMapColors))
      }
    }
    
    if (!mapScenarios){
      mapgo.list <- numeric(length(master_map_list))
      mapunits.list <- character(length(master_map_list))
      nintervals <- numeric(length(master_map_list))
      intervals <- matrix(0,nrow=length(master_map_list),ncol=length(predictionMapColors)+1)
    }else{
      if (!Rshiny){
        master_map_list<-scenario_map_list
        
      }
    }#scenario
    if (Rshiny){#Rshiny
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
      mapsPerPage<-4
      if (input$batch=="Batch"){
        master_map_list<-allMetrics
        if (mapScenarios){
          scenario_map_list<-allMetrics
        }
        output_map_type<-tolower(as.character(input$outCheck))
        
        shinyMapType<-ifelse(input$mapType=="Stream" | 
                                (mapScenarios & regexpr("stream",paste(output_map_type,collapse=","))>0),"stream",
                             "catchment")

        
        if (shinyMapType=="stream" &  input$shapeFile=="yes"){ 
         outputESRImaps[1]<-"yes"
        }
        if (shinyMapType=="catchment" &  input$shapeFile=="yes"){ 
         outputESRImaps[2]<-"yes"
        }
      }else{
        output_map_type<-tolower(as.character(input$outType))
        scenario_map_list<-c(trimws(gsub("-","",input$var)))
        master_map_list<-scenario_map_list
        
        
      }
    }#ewnd Rshiny
    if (mapScenarios){
      mapgo.list <- numeric(length(scenario_map_list))
      mapunits.list <- character(length(scenario_map_list))
      nintervals <- numeric(length(scenario_map_list))
      intervals <- matrix(0,nrow=length(scenario_map_list),ncol=length(scenarioMapColors))
    }
    
    #get geoLines
    existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
    if (existGeoLines){
      load(paste0(path_gis,.Platform$file.sep,"GeoLines"))
    }
    
    existlineShape<-FALSE
    if ((paste(output_map_type,collapse="") %in% c("stream","both") & !Rshiny) | 
        (Rshiny & input$mapType=="Stream" & !mapScenarios) | 
        (Rshiny & regexpr("stream",paste(output_map_type,collapse=","))>0 & mapScenarios)){
      #get lineShape
      existlineShape<-checkBinaryMaps(lineShapeName,path_gis,batch_mode)
      if (existlineShape){
        load(paste0(path_gis,.Platform$file.sep,"lineShape"))
      }
      
    }
    existpolyShape<-FALSE
    if ((paste(output_map_type,collapse="") %in% c("catchment","both") & !Rshiny) | 
        (Rshiny & input$mapType=="Catchment" & !mapScenarios) |
        (Rshiny & regexpr("catchment",paste(output_map_type,collapse=","))>0  & mapScenarios)){
      #get polyShape
      existpolyShape<-checkBinaryMaps(polyShapeName,path_gis,batch_mode)
      if (existpolyShape){
        load(paste0(path_gis,.Platform$file.sep,"polyShape"))
      }
    }
    
    
    commonvar<-"tempID"
    
    #---------------------------------------------------------------#
    # Loop through variable list
    
    
    MAPID <- eval(parse(text=paste0("subdata$","waterid_for_RSPARROW_mapping") ))   # added 3-25-2017
    dmapfinal <- data.frame(MAPID)                                   # added 3-25-2017
    colnames(dmapfinal) <- c(commonvar)
    break1<-list()
    testNA<-list()
    
    
    
    # remove bad variables from master_map_list
    
    if (!mapScenarios){
      master_map_list<-master_map_list[which(!master_map_list %in% noMaplist)]
      testmaster<-character(0)
      if (exists("oparmlist")){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% oparmlist)])}
      if (exists("oyieldlist")){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% oyieldlist)])}
      if (!mapScenarios & !is.na(map_uncertainties[1])){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% map_uncertainties)])}
      testmaster<-c(testmaster,master_map_list[which(master_map_list %in% datalstreq)])
      testmaster<-master_map_list
    }else{
      testmaster<-scenario_map_list
    }
    
    if (length(testmaster)!=0){
      for (k in 1:length(master_map_list)) {
        #set if criteria
        if ((regexpr("ratio_",master_map_list[k])<0 & regexpr("percent_",master_map_list[k])<0)
            | !mapScenarios){ ratioScenario<-FALSE
        }else{
          ratioScenario<-TRUE
        }
        
        # Load matrix
        icolumn<-0
        if (exists("oparmlist")){
          for(i in 1:length(oparmlist)) {
            if (!ratioScenario){
              if(oparmlist[i] == master_map_list[k]) {
                icolumn <- i
                
              }
            }else{#ratio
              ratioMetric<-ifelse(master_map_list[k]=="ratio_total" | master_map_list[k]=="percent_total","pload_total","pload_inc")
              if(oparmlist[i] == ratioMetric) {
                icolumn <- i
              }
            }
          }
          
          if(icolumn>0) {
            if (!ratioScenario){
              vvar <- predmatrix[,icolumn] 
              if (mapScenarios){
                vvar<-ifelse(scenarioFlag==0,NA,vvar)
              }
              
              mapunits <- loadunits[icolumn]
            }else{
              vvar <- predmatrix_chg[,icolumn]
              if (regexpr("ratio_",master_map_list[k])>0){
               mapunits <- "Ratio of updated to baseline metric" 
              }else{
                mapunits <- "Percent of updated to baseline metric"  
              }
              
            }
            
            
          }
        }#exists oparmlist
        
        # Yield matrix  
        if (exists("oyieldlist")){
          if(icolumn == 0) {
            for(i in 1:length(oyieldlist)) {
              if (!ratioScenario){
                if(oyieldlist[i] == master_map_list[k]) {
                  icolumn <- i
                }
                
              }else{#ratio
                ratioMetric<-ifelse(master_map_list[k]=="ratio_total" | 
                                      master_map_list[k]=="percent_total","pload_total","pload_inc")
                if(oyieldlist[i] == ratioMetric) {
                  icolumn <- i
                }
              }
            }
            if(icolumn>0) {
              if (!ratioScenario){
                vvar <- yldmatrix[,icolumn] 
                if (mapScenarios){
                  vvar<-ifelse(scenarioFlag==0,NA,vvar)
                }
                mapunits <- yieldunits[icolumn]
                
              }else{
                vvar <- yldmatrix_chg[,icolumn] 
                
                if (regexpr("ratio_",master_map_list[k])>0){
                  mapunits <- "Ratio of updated to baseline metric" 
                }else{
                  mapunits <- "Percent Change in baseline metric"  
                }
              }
            }
          }
        }#exists oyieldlist
        
        if (!mapScenarios){
          # check list of uncertainties:  map_uncertainties  
          if(icolumn == 0) {
            if(!is.na(map_uncertainties[1])){
              for(i in 1:length(map_uncertainties)) {
                if(map_uncertainties[i] == master_map_list[k]) {
                  icolumn <- i
                  mapunits <- "Percent"
                }
              }
              if(icolumn>0) {
                dname <- paste0("vvar <- BootUncertainties$",map_uncertainties[icolumn])
                eval(parse(text=dname)) 
              }
            }
          }
          
          
          # check required variable list:  datalstreq  
          if(icolumn == 0) {
            for(i in 1:length(datalstreq)) {
              if(datalstreq[i] == master_map_list[k]) {
                icolumn <- i
                mapunits <- datalstunits[icolumn]
              }
            }
            if(icolumn>0) {
              dname <- paste0("vvar <- ",datalstreq[icolumn])
              eval(parse(text=dname)) 
            }
          }
        }
        

        if(icolumn > 0) {
          
          
          
          
          mapgo.list[k] <- icolumn
          mapunits.list[k] <- mapunits
          
          #for output to shapefile
          if (k==1){
            dmapAll<-data.frame(MAPID,vvar)
            names(dmapAll)[1]<-commonvar
          }else{
            dmapAll<-cbind(dmapAll,vvar)
          }
          names(dmapAll)[length(dmapAll)]<-master_map_list[k]
          

          
          
          ###vvar2 must be considered for scenario mapping###
          
          #aggregate seasons or years if required
          aggFuncs<-c("mean","median","min","max")
          if (!is.na(map_seasons) & map_seasons %in% aggFuncs & !is.na(map_years) & map_years %in% aggFuncs){
            if (map_seasons!=map_years){
              message("ERROR : CANNOT AGGREGATE YEARS AND SEASONS WITH DIFFERENT FUNCTIONS")
              errorOccurred("predictMaps.R",batch_mode)
            }
          }
          
     
            
           add_map.list<-aggDynamicMapdata(map_years,map_seasons,enable_plotlyMaps,add_plotlyVars,
                                          aggFuncs,vvar,MAPID,commonvar,subdata)
          unPackList(lists = list(add_map.list = add_map.list),
                     parentObj = list(NA)) 
          
          
          
          

          
          # check for NAs
          eval(parse(text = paste0("testNA$",master_map_list[k],"<-length(vvar[which(is.na(vvar))])")))
          testNAvar<- eval(parse(text = paste0("testNA$",master_map_list[k])))
          
          if (testNAvar!=0){
            vvar1<-vvar[which(is.na(vvar))]
            vvar2<-na.omit(vvar)
          }
          ###############test 1 class
          
          
          if (mapScenarios & (regexpr("ratio_",master_map_list[k])>0 | regexpr("percent_",master_map_list[k])>0)){
            
            vvar1 <- vvar[vvar==1]
            vvar2 <- vvar[vvar!=1]
            
          }
          
          #set breakpoints
          if (!ratioScenario){
            
            #set colors
            if (mapScenarios){
              Mcolors<-scenarioMapColors[2:length(scenarioMapColors)]
            }else{
              Mcolors<-predictionMapColors
            }
            
            if (testNAvar==0){
              brks<-mapBreaks(vvar,Mcolors)$brks
              uniqueBrks <- unique(brks) # define quartiles
              
              
              
              if (length(brks)>=2){
                
                qvars <- as.integer(cut(vvar, brks, include.lowest=TRUE))  # classify variable
                
                Mcolors <- Mcolors[1:(length(brks)-1)]
                nintervals[k] <- length(uniqueBrks)-1
              }else{
                if (!is.na(brks)){
                  qvars <- as.integer(cut(vvar, brks, include.lowest=TRUE))  # classify variable
                  
                  
                }else{
                  qvars<-rep(1,length(vvar))
                  uniqueBrks<-unique(vvar)
                  
                }              
                Mcolors <- Mcolors[1:1]
                nintervals[k] <- length(uniqueBrks)
              }
              # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
              MAPCOLORS <- Mcolors[qvars] 
              
            }else{#testNAvar!=0
              chk1 <- mapBreaks(vvar2,Mcolors)$brks
              iprob<-mapBreaks(vvar2,Mcolors)$iprob
              chk <- unique(chk1) # define quartiles with values of 1.0 removed
              qvars<-as.integer(cut(as.numeric(vvar), chk1, include.lowest=TRUE))
              qvars<-ifelse(is.na(qvars),0,qvars)
              qvars<-qvars+1
              if (!mapScenarios){
                Mcolors <- c("gray",Mcolors)
              }else{
                Mcolors<-scenarioMapColors
              }
              
              Mcolors <- Mcolors[1:(length(chk1)+1)]
              
              
              
              # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
              MAPCOLORS <- Mcolors[qvars]
            }
            
          }else{#ratio plot
            if (length(vvar2)!=0){
              chk1 <- mapBreaks(vvar2,scenarioMapColors[1:(length(scenarioMapColors)-1)])$brks
              iprob<-mapBreaks(vvar2,scenarioMapColors[1:(length(scenarioMapColors)-1)])$iprob
              chk <- unique(chk1) # define quartiles with values of 1.0 removed
              
              chk[iprob+1] <- chk[iprob+1]+1
              qvar1 <- vvar
              qvar1[ qvar1 == 1 ] <- 9999   # code ratios=1 separately
              if (iprob!=0){
                for (i in 1:iprob) {
                  qvar1[ qvar1 >= chk[i] & qvar1 < chk[i+1] ] <- 9999+i
                }
                max <- 9999+i+2
                qvar1[ qvar1 == 9999] <- 1       # code values of 1.0
                for (i in 2:(iprob+1)) {         # reverse code to associate largest reductions with hottest colors
                  qvar1[ qvar1 == (max-i)] <- i
                }
                chk[iprob+1] <- chk[iprob+1]-1
                
                Mcolors <- scenarioMapColors
                Mcolors <- Mcolors[1:(length(chk1)+1)]
                
                # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
                MAPCOLORS <- Mcolors[qvar1]
              }else{
                MAPCOLORS<-rep(scenarioMapColors[1],length(MAPID))
              }
            }else{#no change scenario
              MAPCOLORS<-rep(scenarioMapColors[1],length(MAPID))
              chk<-1
              iprob<-0
            }

            if (regexpr("percent_",master_map_list[k])>0){
             
            chk<-(chk-1)*100
            vvar<-(vvar-1)*100
            #iprob<-(1-iprob)*100

              }
          }#end ratio plot
          
         # dmap <- data.frame(MAPID,as.character(MAPCOLORS))   # ,vvar)# added 3-25-2017
          dmap<-mapdata
          dmap$color <- as.character(MAPCOLORS)
          mapvarname <- paste0("MAPCOLORS",k)
          names(dmap)[names(dmap)=="color"]<-mapvarname
          
          dmap$vvar<-vvar
          mapdataname<-paste0("vvar",k)
          names(dmap)[names(dmap)=="vvar"]<-mapdataname
          

          if ((!mapScenarios | (regexpr("ratio_",master_map_list[k])<0 & regexpr("percent_",master_map_list[k])<0)) & testNAvar==0){
            intervals[k,1:length(uniqueBrks)] <- uniqueBrks
          }else{
            intervals[k,1:length(chk)] <- chk
            nintervals[k] <- iprob+1

          }
          
          
          eval(parse(text=paste0("break1$",master_map_list[k],"<-as.character(intervals[1:nintervals[k]])")))
          if (!ratioScenario){
            if (testNAvar==0){
              if (length(unique(vvar))!=1){
                for (i in 1:nintervals[k]) {
                  break1[k][[1]][i] <- paste0(round(intervals[k,i],digit=predictionClassRounding)," TO ",round(intervals[k,i+1],digit=predictionClassRounding))
                }
              }else{
                break1[k][[1]][1] <- paste0(round(unique(vvar),digit=predictionClassRounding)," TO ",round(unique(vvar),digit=predictionClassRounding))
              }
            }else{#testNAvar!=0
              if (!mapScenarios){
                break1[k][[1]][1] <- 'NA'
              }else{
                break1[k][[1]][1] <- 'No Change'
              }
              
              j<-1
              for (i in 2:nintervals[k]) {
                j <- j+1
                break1[k][[1]][j] <- paste0(round(intervals[k,i-1],digit=predictionClassRounding)," TO ",round(intervals[k,i],digit=predictionClassRounding))
              }
            }
          }else{
            if (regexpr("percent_",master_map_list[k])>0){
              break1[k][[1]][1] <- '0 (No Change)'
              }else{
              break1[k][[1]][1] <- '1.0 (No Change)'
            }
            
            if (length(vvar2)!=0){
              j<-1
              for (i in (nintervals[k]):2) {
                j <- j+1
                break1[k][[1]][j] <- paste0(round(intervals[k,i-1],digit=predictionClassRounding)," TO ",round(intervals[k,i],digit=predictionClassRounding))
              }
            }#legnth(vvar2)!=0
          }
          
          
          nlty <-rep(1,nintervals[k])
          nlwd <- rep(lineWidth,nintervals[k])
          
        }
        
        if(mapgo.list[k] > 0){
          
          
          dmapfinal <- merge(dmapfinal,dmap,by=names(mapdata)[names(mapdata)!="vvar"])
          mapvarname <- paste0("dmapfinal$MAPCOLORS",k," <- as.character(dmapfinal$MAPCOLORS",k,")")
          eval(parse(text=mapvarname))

        }
      } # end variable loop
      
      #------------------------------------------------------------#     
      
     

          subdataMerge<-merge(dmapfinal,uniqueSubdata,by.x = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)], 
                            by.y = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)])
        
          subdataMerge<-subdataMerge[,names(subdataMerge) %in% c(names(subdata),commonvar)]
          
          
        if ((enable_plotlyMaps!="no" & enable_plotlyMaps!="static") | !is.na(add_plotlyVars[1])){
        
        if ((!is.na(map_seasons) & !map_seasons %in% aggFuncs) & (!is.na(map_years) & !map_years %in% aggFuncs)){
          names(subdataMerge)[names(subdataMerge)==commonvar]<-"waterid_for_RSPARROW_mapping"
          subdataMerge<-subdataMerge[,names(subdataMerge) %in% names(subdata)]
          
        if (is.na(map_years) & is.na(map_seasons)){
        dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon")), dmapfinal, subdataMerge)$mapData
        }else if (is.na(map_seasons)){
          dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon","year","mapping_waterid")), dmapfinal, subdataMerge)$mapData
        }else if (is.na(map_years)){
          dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon","season","mapping_waterid")), dmapfinal, subdataMerge)$mapData
        }else{
          dmapfinal<-addMarkerText("",unique(c(add_plotlyVars,"lat","lon","year","season","mapping_waterid")), dmapfinal, subdataMerge)$mapData
        }
        
          }else if ((!is.na(map_years) & map_years %in% aggFuncs) | (!is.na(map_seasons) & map_seasons %in% aggFuncs)){
          uniqueSubdata$mapping_waterid<-eval(parse(text=paste0("uniqueSubdata$",commonvar)))  
          dmapfinal<-merge(dmapfinal,uniqueSubdata,by.x = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)],
                           by.y = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)])
        
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
          dmapfinal<-merge(dmapfinal,uniqueSubdata,by.x = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)],
                           by.y = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)])
          dmapfinal<-dmapfinal[,names(dmapfinal) %in% NAMES]
          
        }
          
      }else if (((!is.na(map_seasons) & map_seasons %in% aggFuncs) | (!is.na(map_years) & map_years %in% aggFuncs)) & is.na(add_plotlyVars[1])){
        uniqueSubdata$mapping_waterid<-eval(parse(text=paste0("uniqueSubdata$",commonvar)))
        dmapfinal<-merge(dmapfinal,uniqueSubdata,by.x = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)], 
                            by.y = names(dmapfinal)[names(dmapfinal) %in% names(uniqueSubdata)]) 
        }
        
        
      
      # merge selected variables to the shape file\
      if ((paste(output_map_type,collapse="") %in% c("stream","both") & !Rshiny) | 
          (Rshiny & input$mapType=="Stream" & !mapScenarios) | 
          (Rshiny & regexpr("stream",paste(output_map_type,collapse=","))>0 & mapScenarios)){

        commonvar <- lineWaterid
        names(dmapfinal)[1]<-commonvar
        names(dmapAll)[1]<-commonvar

        #output shapefile
        if (outputESRImaps[1]=="yes"){
          if (is.na(map_years) & is.na(map_seasons)){
            lineShape2 <- merge(lineShape, dmapfinal, by.x = commonvar, by.y = commonvar)
          }else{
            lineShape2 <- merge(lineShape, dmapfinal, by.x = commonvar, by.y = "mapping_waterid") 
          }
          lineShape2<-lineShape2[,which(regexpr("MAPCOLORS",names(lineShape2))<0)]

          if (!Rshiny){
            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
            }
            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep),showWarnings = FALSE)
            }
            
             
               suppressWarnings(unlink(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",
                            .Platform$file.sep,"prediction",.Platform$file.sep), recursive = TRUE))
            
           st_write(lineShape2, paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",
                           .Platform$file.sep,"prediction",.Platform$file.sep,"lineShape.shp"))
            
          }else if (mapScenarios & input$batch=="Batch"){
            if (!dir.exists(paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
              dir.create(paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
            }
            
             
            suppressWarnings(unlink(paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",
                           .Platform$file.sep,"prediction",.Platform$file.sep), recursive = TRUE))
            
           st_write(lineShape2, paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",
                                      .Platform$file.sep,"prediction",.Platform$file.sep,"lineShape.shp"))
            
          }else if (input$batch=="Batch"){

            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
            }
            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep),showWarnings = FALSE)
            }
            
             
            suppressWarnings(unlink(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",
                           .Platform$file.sep,"prediction",.Platform$file.sep), recursive = TRUE))
            
           st_write(lineShape2, paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",
                                      .Platform$file.sep,"prediction",.Platform$file.sep,"lineShape.shp"))
            }
        }#end output esri stream

        # loop through each of the variables...
        for (k in 1:length(master_map_list)) {
          testNAvar<-eval(parse(text = paste0("testNA$",master_map_list[k])))
          if (!mapScenarios | regexpr("ratio_",master_map_list[k])<0 & regexpr("percent_",master_map_list[k])<0){
            #set up colors
            if (!mapScenarios){
              Mcolors <- predictionMapColors
            }else{
              Mcolors<-scenarioMapColors[2:length(scenarioMapColors)]
            }
            #set NA class
            if (testNAvar!=0){
              if (!mapScenarios){
                Mcolors <- c("gray",Mcolors)
              }else{
                Mcolors<-scenarioMapColors
              }
            }
            
          }else{
            Mcolors <- scenarioMapColors
          }
          
          #output maps
          if (!Rshiny){
            input$button<-""
          }
          if (((input$batch=="Batch" & Rshiny) | (input$button=="savePDF" & Rshiny) | !Rshiny)){
            if (!mapScenarios){
              if (Rshiny){
                filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Stream",.Platform$file.sep,run_id,"_",master_map_list[k],".pdf")
              }else{
                if (!dir.exists(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Stream"))){
                  dir.create(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Stream"))
                }
                filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Stream",.Platform$file.sep,run_id,"_",master_map_list[k],".pdf")
              }            
              }else{
                if (!Rshiny){
                  input$scenarioName<-scenario_name
                }
              if (!dir.exists(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName))){
                dir.create(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName),showWarnings = FALSE)
              }
              if (!dir.exists(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Stream",.Platform$file.sep))){
                dir.create(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Stream",.Platform$file.sep),showWarnings = FALSE)
              }
              filename<- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Stream",.Platform$file.sep,
                               input$scenarioName,"_",run_id,"_",scenario_map_list[k],".pdf")
      
            }
           # pdf(filename)
          }#end if create filename      
          
          reportPath<-paste0(path_master,"outputMaps.Rmd")
          
          #create plot sequence
          plots<-setupDynamicMaps(dmapfinal,map_years,map_seasons,mapPageGroupBy,mapsPerPage, Rshiny, enable_plotlyMaps)
          if ((input$batch=="Batch" & Rshiny) | !Rshiny){
          }else{
            mapdataname <- paste0("vvar",k) 
          }
          mapLoopInput.list<- list(
                file.output.list = file.output.list,
                GeoLines = GeoLines,
                plotShape = lineShape,
                dmapfinal = dmapfinal,
                plots = plots,
                k = k,
                existGeoLines = existGeoLines,
                Rshiny = Rshiny,
                input = input,
                predictionTitleSize = predictionTitleSize,
                scenario_name = scenario_name,
              scenario_map_list = scenario_map_list,
              master_map_list = master_map_list,
              predictionLegendSize = predictionLegendSize,
                mapunits.list = mapunits.list,
                predictionLegendBackground = predictionLegendBackground,
                break1 = break1,
                Mcolors = Mcolors,
                enable_plotlyMaps = enable_plotlyMaps,
                output_map_type = output_map_type,
                lineWidth = lineWidth,
                lon_limit = lon_limit,
                lat_limit = lat_limit,
                nlty = nlty,
                nlwd = nlwd,
                CRStext = mapping.input.list$CRStext,
                mapdataname = mapdataname,
                predictionMapColors = predictionMapColors,
                add_plotlyVars = add_plotlyVars,
                mapScenarios = mapScenarios,
                predictionMapBackground = predictionMapBackground,
                LineShapeGeo = LineShapeGeo,
                mapvarname = mapvarname,
                predictionClassRounding = predictionClassRounding,
                commonvar = commonvar,
                map_years = map_years,
                map_seasons = map_seasons,
                mapsPerPage = mapsPerPage,
                mapPageGroupBy = mapPageGroupBy,
                aggFuncs = aggFuncs
              )
         

          if ((input$batch=="Batch" & Rshiny) | !Rshiny){
            if (!existGeoLines){GeoLines<-NA}
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



            }else{#Rhiny interactive or enable_plotlyMaps==no

              mapType <- "stream"
              #mapdataname <- paste0("vvar",k)
              map_loop.list<-mapLoopStr(mapType,mapLoopInput.list)
              
              
              unPackList(lists = list(map_loop.list = map_loop.list),
                         parentObj = list(NA))

            
              if (Rshiny){
                return(pa)
              }

              
            }#end Rshiny interactive
        }#end variable loop
        

       
        
        
        
      }
      

      if (((paste(output_map_type,collapse="") %in% c("catchment","both") & !Rshiny) | 
           (Rshiny & input$mapType=="Catchment" & !mapScenarios) |
           (Rshiny & regexpr("catchment",paste(output_map_type,collapse=","))>0  & mapScenarios)) & existpolyShape) {
       
        commonvar <- polyWaterid
        names(dmapfinal)[1]<-commonvar
        names(dmapAll)[1]<-commonvar


         #output shapefile
        if (outputESRImaps[2]=="yes"){
          if (is.na(map_years) & is.na(map_seasons)){
            polyShape2 <- merge(polyShape, dmapfinal, by.x = commonvar, by.y = commonvar)
          }else{
            polyShape2 <- merge(polyShape, dmapfinal, by.x = commonvar, by.y = "mapping_waterid")
          }
          polyShape2<-polyShape2[,which(regexpr("MAPCOLORS",names(polyShape2))<0)]
          
          if (!Rshiny){
            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
            }
            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep),showWarnings = FALSE)
            }
            suppressWarnings(unlink(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",
                                           .Platform$file.sep,"prediction",.Platform$file.sep), recursive = TRUE))
            st_write(polyShape2, paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,
                                      "ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"polyShape.shp"))
          
            
          }else if (mapScenarios  & input$batch=="Batch"){
            if (!dir.exists(paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
              dir.create(paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
            }
            suppressWarnings(unlink(paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",
                                           .Platform$file.sep,"prediction",.Platform$file.sep), recursive = TRUE))
            st_write(polyShape2, paste0(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,
                                      "ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"polyShape.shp"))
          }else if (input$batch=="Batch"){
            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
            }
            if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep))){
              dir.create(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep),showWarnings = FALSE)
            }
            suppressWarnings(unlink(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",
                                           .Platform$file.sep,"prediction",.Platform$file.sep), recursive = TRUE))
            st_write(polyShape2, paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,
                                      "ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"polyShape.shp"))}
        }

        
        # loop through each of the variables...
        for (k in 1:length(master_map_list)) {
          
          
          if (!Rshiny){
            input$button<-""
          }
          if (((input$batch=="Batch" & Rshiny) | (input$button=="savePDF" & Rshiny) | !Rshiny)){
            if (!mapScenarios){
              if (Rshiny){
                             filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Catchment",.Platform$file.sep,run_id,"_",master_map_list[k],".pdf")
              }else{
                if (!dir.exists(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Catchment"))){
                  dir.create(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Catchment"))
                }
                filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Catchment",.Platform$file.sep,run_id,"_",master_map_list[k],".pdf")
              }
            }else{
              if (!Rshiny){
                input$scenarioName<-scenario_name
              }
              if (!dir.exists(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName))){
                dir.create(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName),showWarnings = FALSE)
              }
              if (!dir.exists(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Catchment",.Platform$file.sep))){
                dir.create(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Catchment",.Platform$file.sep),showWarnings = FALSE)
              }
              filename<- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Catchment",.Platform$file.,
                               input$scenarioName,"_",run_id,"_",scenario_map_list[k],".pdf")
             
              }
            

          }
          reportPath<-paste0(path_master,"outputMaps.Rmd")
          plots<-setupDynamicMaps(dmapfinal,map_years,map_seasons,mapPageGroupBy,mapsPerPage, Rshiny, enable_plotlyMaps)
          if ((input$batch=="Batch" & Rshiny) | !Rshiny){
          }else{
            mapdataname <- paste0("vvar",k) 
          }
          mapLoopInput.list<- list(
            file.output.list = file.output.list,
            GeoLines = GeoLines,
            plotShape = polyShape,
            dmapfinal = dmapfinal,
            plots = plots,
            k = k,
            existGeoLines = existGeoLines,
            Rshiny = Rshiny,
            input = input,
            predictionTitleSize = predictionTitleSize,
            scenario_name = scenario_name,
            scenario_map_list = scenario_map_list,
            master_map_list = master_map_list,
            predictionLegendSize = predictionLegendSize,
            mapunits.list = mapunits.list,
            predictionLegendBackground = predictionLegendBackground,
            break1 = break1,
            Mcolors = Mcolors,
            enable_plotlyMaps = enable_plotlyMaps,
            output_map_type = output_map_type,
            lineWidth = lineWidth,
            lon_limit = lon_limit,
            lat_limit = lat_limit,
            nlty = nlty,
            nlwd = nlwd,
            CRStext = mapping.input.list$CRStext,
            mapdataname = mapdataname,
            predictionMapColors = predictionMapColors,
            add_plotlyVars = add_plotlyVars,
            mapScenarios = mapScenarios,
            predictionMapBackground = predictionMapBackground,
            LineShapeGeo = LineShapeGeo,
            mapvarname = mapvarname,
            predictionClassRounding = predictionClassRounding,
            commonvar = commonvar,
            map_years = map_years,
            map_seasons = map_seasons,
            mapsPerPage = mapsPerPage,
            mapPageGroupBy = mapPageGroupBy,
            aggFuncs = aggFuncs
          )
   
          # if (((input$batch=="Batch" & Rshiny) |
          #      !Rshiny) & (enable_plotlyMaps!="static" & enable_plotlyMaps!="no")){
          if (((input$batch=="Batch" & Rshiny) | !Rshiny)){
            if (!existGeoLines){GeoLines<-NA}
            htmlFile<-gsub("pdf","html",filename)
            
            rmdTitle<-file.output.list$run_id
            path_outputMapsChild<-file_path_as_absolute(paste0(path_master,"outputMapsChild.Rmd"))
            
            rmarkdown::render(paste0(path_master,"outputMaps.Rmd"),
                              params = list(
                                rmdTitle = rmdTitle,
                                mapType = "catchment",
                                mapLoopInput.list = mapLoopInput.list,
                                path_outputMapsChild = path_outputMapsChild
                              ),
                              output_file = htmlFile, quiet = TRUE
            )
              
            
          }else{#Rhiny interactive

            mapType <- "catchment"
           # mapdataname <- paste0("vvar",k)
            map_loop.list<-mapLoopStr(mapType,mapLoopInput.list)
            
            
            unPackList(lists = list(map_loop.list = map_loop.list),
                       parentObj = list(NA))
            

            
            if (Rshiny){
              return(pa)
            }
          }#end Rshiny interactive
          ######################
          ######################
          #######################
 
    #      
        }#end variable loop

        
       
      }
    }else {#if length(master_map_list)
      message(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
      if (batch_mode=="yes"){
        cat(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
      }
    }
  } else {
    message(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
    if (batch_mode=="yes"){
      cat(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
    }
  }#end if file.exists(predict.list) or if_predict="yes"
  
  
}#end function
