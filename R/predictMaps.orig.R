predictMaps<-function(#Rshiny
                      input,allMetrics,output_map_type, Rshiny, 
                      #regular
                      path_results,file_sum,path_gis,
                      #map_uncertainties,BootUncertainties,
                      data_names,mapping.input.list,
                      #predict.list,
                      subdata,
                      #scenarios
                      mapScenarios,
                      scenario_map_list,
                      predictScenarios.list,
                      scenario_name,
                      batch_mode,ErrorOccured) {
 #Rshiny input
  #input$batch
  #input$scenarioName
  #input$mapType
  #input$var

  if (ErrorOccured=="no"){
    tryIt<-try({ 
      # obtain uncertainties, if available
      objfile <- paste(path_results,"/predict/",file_sum,"_BootUncertainties",sep="")
      if(file.exists(objfile) == TRUE) {
        load(objfile)
        map_uncertainties <- c("se_pload_total","ci_pload_total")
      } else {
        map_uncertainties <- NA 
        BootUncertainties <- NA
      }

      testList<-character(0)
      if (mapScenarios==FALSE){
      #test if mapped variable not prediction or scenario
      if (Rshiny==TRUE){ #shiny
        if (input$batch=="Interactive"){
          master_map_list<-c(trimws(gsub("-","",input$var)))
    
        }else{
          master_map_list<-c(trimws(gsub("-","",allMetrics)))
        } 
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
            
            #testList<-testList[which(testList!=l)]
            #noMaplist<-c(noMaplist,l)
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
          if (testNum!="numeric"){
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
      
      if ((file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep="")) & mapScenarios==FALSE) | 
          mapScenarios==TRUE |
          (length(testList)>0 & mapScenarios==FALSE)){
        if (mapScenarios==FALSE & file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))){
        if (!exists("predict.list")){
          load(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))
        }
        }

        
      # Setup variable lists 
      # create global variable from list names (mapping.input.list)
      for(i in 1:length(mapping.input.list)){
        tempobj=mapping.input.list[[i]]
        eval(parse(text=paste(names(mapping.input.list)[[i]],"= tempobj")))
      }
        
        if (mapScenarios==FALSE & exists("predict.list")){
      # create global variable from list names (predict.list)
      
          for(i in 1:length(predict.list)){
        tempobj=predict.list[[i]]
        eval(parse(text=paste(names(predict.list)[[i]],"= tempobj")))
      }
        }else if (mapScenarios==TRUE){
          # create global variable from list names (predict.list)
          for(i in 1:length(predictScenarios.list)){
            tempobj=predictScenarios.list[[i]]
            eval(parse(text=paste(names(predictScenarios.list)[[i]],"= tempobj")))
          }
        }else if (!exists("predict.list")){
          master_map_list<-master_map_list[which(master_map_list %in% names(subdata))]
        }
        
      # required names
      datalstreq <- data_names$sparrowNames
      datalstunits <- data_names$varunits
      
      # transfer required variables to global environment from SUBDATA
      for (i in 1:length(datalstreq)) {
        dname <- paste("subdata$",datalstreq[i],sep="") 
        x1name <- paste(datalstreq[i],sep="")
        if((x1name %in% names(subdata)) == TRUE) {
          assign(datalstreq[i],eval(parse(text=dname)))
        }
      }

if (Rshiny==TRUE){ #shiny
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
  if (mapScenarios==FALSE){
        predictionMapColors<-eval(parse(text=input$predictionMapColors))
        }else{
          scenarioMapColors<-eval(parse(text=input$scenarioMapColors))
        }
      }
      
      if (mapScenarios==FALSE){
      mapgo.list <- numeric(length(master_map_list))
      mapunits.list <- character(length(master_map_list))
      nintervals <- numeric(length(master_map_list))
      intervals <- matrix(0,nrow=length(master_map_list),ncol=length(predictionMapColors)+1)
      }else{
        if (Rshiny==FALSE){
          master_map_list<-scenario_map_list
        }else if (input$batch=="Batch"){
          master_map_list<-allMetrics
          scenario_map_list<-allMetrics
          output_map_type<-tolower(as.character(input$outCheck))
          if ((input$mapType=="Stream" | (mapScenarios==TRUE & regexpr("stream",paste(output_map_type,collapse=","))>0)) & input$shapeFile=="yes"){
            outputERSImaps[1]<-"yes"
          }
          if ((input$mapType=="Catchment" | (mapScenarios==TRUE & regexpr("catchment",paste(output_map_type,collapse=","))>0)) & input$shapeFile=="yes"){
            outputERSImaps[2]<-"yes"
          }
        }else{
          output_map_type<-tolower(as.character(input$outType))
         # master_map_list<-c(trimws(gsub("-","",input$var)))
          scenario_map_list<-"pload_total"
          master_map_list<-scenario_map_list

          
        }

        mapgo.list <- numeric(length(scenario_map_list))
        mapunits.list <- character(length(scenario_map_list))
        nintervals <- numeric(length(scenario_map_list))
        intervals <- matrix(0,nrow=length(scenario_map_list),ncol=length(scenarioMapColors))
      }
  
      #get geoLines
      existGeoLines<-checkBinaryMaps("LineShapeGeo", LineShapeGeo,"GeoLines", path_gis,batch_mode,ErrorOccured)
      if (existGeoLines==TRUE){
        load(paste(path_gis,"/GeoLines",sep=""))
      }
      
      existlineShape<-FALSE
      if ((paste(output_map_type,collapse="") %in% c("stream","both") & Rshiny==FALSE) | 
          (Rshiny==TRUE & input$mapType=="Stream" & mapScenarios==FALSE) | 
          (Rshiny==TRUE & regexpr("stream",paste(output_map_type,collapse=","))>0 & mapScenarios==TRUE)){
      #if ((output_map_type %in% c("stream","both") & Rshiny==FALSE) | (Rshiny==TRUE & (input$mapType=="Stream" | mapScenarios==TRUE))){
        #get lineShape
      existlineShape<-checkBinaryMaps("lineShapeName", lineShapeName,"lineShape", path_gis,batch_mode,ErrorOccured)
      if (existlineShape==TRUE){
        load(paste(path_gis,"/lineShape",sep=""))
      }

      }
      existpolyShape<-FALSE
      if ((paste(output_map_type,collapse="") %in% c("catchment","both") & Rshiny==FALSE) | 
          (Rshiny==TRUE & input$mapType=="Catchment" & mapScenarios==FALSE) |
          (Rshiny==TRUE & regexpr("catchment",paste(output_map_type,collapse=","))>0  & mapScenarios==TRUE)){
     # if ((output_map_type %in% c("catchment","both") & Rshiny==FALSE) | (Rshiny==TRUE & input$mapType=="Catchment")){
        #get polyShape
        existpolyShape<-checkBinaryMaps("polyShapeName", polyShapeName,"polyShape", path_gis,batch_mode,ErrorOccured)
        if (existpolyShape==TRUE){
          load(paste(path_gis,"/polyShape",sep=""))
        }
      }
      

      commonvar<-"tempID"
      
      #---------------------------------------------------------------#
      # Loop through variable list
      

      MAPID <- eval(parse(text=paste("subdata$","waterid_for_RSPARROW_mapping",sep="") ))   # added 3-25-2017
      dmapfinal <- data.frame(MAPID)                                   # added 3-25-2017
      colnames(dmapfinal) <- c(commonvar)
      break1<-list()
      testNA<-list()
    
      
      
      # remove bad variables from master_map_list

  if (mapScenarios==FALSE){
      master_map_list<-master_map_list[which(!master_map_list %in% noMaplist)]
            testmaster<-character(0)
      if (exists("oparmlist")){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% oparmlist)])}
      if (exists("oyieldlist")){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% oyieldlist)])}
      if (mapScenarios==FALSE & !is.na(map_uncertainties[1])){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% map_uncertainties)])}
      testmaster<-c(testmaster,master_map_list[which(master_map_list %in% datalstreq)])
      testmaster<-master_map_list
  }else{
    testmaster<-scenario_map_list
  }
      if (length(testmaster)!=0){
      for (k in 1:length(master_map_list)) {
        
        # Load matrix
        icolumn<-0
        if (exists("oparmlist")){
        for(i in 1:length(oparmlist)) {
          if(oparmlist[i] == master_map_list[k]) {
            icolumn <- i
          }
        }
        if(icolumn>0) {
          if (mapScenarios==FALSE){
           vvar <- predmatrix[,icolumn] 
          mapunits <- loadunits[icolumn]
          }else{
            vvar <- predmatrix_chg[,icolumn]
            mapunits <- "Ratio of updated to baseline metric"
          }
          
          
        }
        }#exists oparmlist
        
        # Yield matrix  
        if (exists("oyieldlist")){
        if(icolumn == 0) {
          for(i in 1:length(oyieldlist)) {
            if(oyieldlist[i] == master_map_list[k]) {
              icolumn <- i
            }
          }
          if(icolumn>0) {
            if (mapScenarios==FALSE){
            vvar <- yldmatrix[,icolumn] 
            mapunits <- yieldunits[icolumn]
            }else{
              vvar <- yldmatrix_chg[,icolumn] 
              mapunits <- "Ratio of updated to baseline metric"
            }
          }
        }
        }#exists oyieldlist
        
       if (mapScenarios==FALSE){
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
              dname <- paste("vvar <- BootUncertainties$",map_uncertainties[icolumn],sep="")
              eval(parse(text=dname)) 
            }
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
            dname <- paste("vvar <- ",datalstreq[icolumn],sep="")
            eval(parse(text=dname)) 
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
          
          # check for NAs
          eval(parse(text = paste0("testNA$",master_map_list[k],"<-length(vvar[which(is.na(vvar))])")))
          testNAvar<- eval(parse(text = paste0("testNA$",master_map_list[k])))
         # testNA<-length(vvar[which(is.na(vvar))])
         # vvar<-ifelse(is.na(vvar),0.0,vvar)
          if (testNAvar!=0){
            vvar1<-vvar[which(is.na(vvar))]
            vvar2<-na.omit(vvar)
          }
          ###############test 1 class
       # if (master_map_list[k]=="deliv_frac"){
      #    vvar<-rep(1,length(vvar))
      #  }
          
          if (mapScenarios==TRUE){
            vvar1 <- vvar[vvar==1]
            vvar2 <- vvar[vvar!=1]
          }
          
          

          #set breakpoints
          if (mapScenarios==FALSE){
            if (testNAvar==0){
              
          brks<-mapBreaks(vvar,predictionMapColors,batch_mode,ErrorOccured)$brks
          uniqueBrks <- unique(brks) # define quartiles
   
          Mcolors <- predictionMapColors
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
              chk1 <- mapBreaks(vvar2,predictionMapColors,batch_mode,ErrorOccured)$brks
              iprob<-mapBreaks(vvar2,predictionMapColors,batch_mode,ErrorOccured)$iprob
              chk <- unique(chk1) # define quartiles with values of 1.0 removed
              qvars<-as.integer(cut(as.numeric(vvar), chk1, include.lowest=TRUE))
                qvars<-ifelse(is.na(qvars),0,qvars)
                qvars<-qvars+1

     
              Mcolors <- c("gray",predictionMapColors)
              Mcolors <- Mcolors[1:(length(chk1)+1)]

              
              
              # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
              MAPCOLORS <- Mcolors[qvars]
}
                     
          }else{
            chk1 <- mapBreaks(vvar2,scenarioMapColors[1:(length(scenarioMapColors)-1)],batch_mode,ErrorOccured)$brks
            iprob<-mapBreaks(vvar2,scenarioMapColors[1:(length(scenarioMapColors)-1)],batch_mode,ErrorOccured)$iprob
            chk <- unique(chk1) # define quartiles with values of 1.0 removed
            
            chk[iprob+1] <- chk[iprob+1]+1
            qvar1 <- vvar
            qvar1[ qvar1 == 1 ] <- 9999   # code ratios=1 separately
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
            
          }
            dmap <- data.frame(MAPID,as.character(MAPCOLORS))   # ,vvar)    # added 3-25-2017
            
            mapvarname <- paste("MAPCOLORS",k,sep="")
            colnames(dmap) <- c(commonvar,mapvarname) # ,master_map_list[k])
            if (mapScenarios==FALSE & testNAvar==0){
            intervals[k,1:length(uniqueBrks)] <- uniqueBrks
            }else{
              intervals[k,1:length(chk)] <- chk
              nintervals[k] <- iprob+1
            
            }
            
            
            eval(parse(text=paste("break1$",master_map_list[k],"<-as.character(intervals[1:nintervals[k]])",sep="")))
            if (mapScenarios==FALSE){
              if (testNAvar==0){
              if (length(unique(vvar))!=1){
              for (i in 1:nintervals[k]) {
              break1[k][[1]][i] <- paste(round(intervals[k,i],digit=predictionClassRounding)," TO ",round(intervals[k,i+1],digit=predictionClassRounding),sep="")
              }
              }else{
                break1[k][[1]][1] <- paste(round(unique(vvar),digit=predictionClassRounding)," TO ",round(unique(vvar),digit=predictionClassRounding),sep="")
              }
              }else{#testNAvar!=0
                break1[k][[1]][1] <- 'NA'
                j<-1
                for (i in 2:nintervals[k]) {
                  j <- j+1
                  break1[k][[1]][j] <- paste(round(intervals[k,i-1],digit=predictionClassRounding)," TO ",round(intervals[k,i],digit=predictionClassRounding),sep="")
                }
              }
          }else{
              break1[k][[1]][1] <- '1.0 (No Change)'
              j<-1
              for (i in (nintervals[k]):2) {
                j <- j+1
                break1[k][[1]][j] <- paste(round(intervals[k,i-1],digit=predictionClassRounding)," TO ",round(intervals[k,i],digit=predictionClassRounding),sep="")
              }
              
            }

            
            nlty <-rep(1,nintervals[k])
            nlwd <- rep(lineWidth,nintervals[k])
          
        }
        if(mapgo.list[k] > 0){

           
         dmapfinal <- merge(dmapfinal,dmap,by=commonvar)
          mapvarname <- paste("dmapfinal$MAPCOLORS",k," <- as.character(dmapfinal$MAPCOLORS",k,")",sep="")
          eval(parse(text=mapvarname))
   
        }
      } # end variable loop
   
      #------------------------------------------------------------#     
      #if (mapScenarios==FALSE){
      #  if (testNA==0){
      #   Mcolors <- predictionMapColors  
      #  }else{
      #  Mcolors <- c("gray",predictionMapColors)}
      #}else{
      #  Mcolors <- scenarioMapColors
      #}

      # merge selected variables to the shape file\
      if ((paste(output_map_type,collapse="") %in% c("stream","both") & Rshiny==FALSE) | 
          (Rshiny==TRUE & input$mapType=="Stream" & mapScenarios==FALSE) | 
          (Rshiny==TRUE & regexpr("stream",paste(output_map_type,collapse=","))>0 & mapScenarios==TRUE)){
        commonvar <- lineWaterid
        names(dmapfinal)[1]<-commonvar
        names(dmapAll)[1]<-commonvar
      lineShape <- sp::merge(lineShape, dmapfinal, by.x = commonvar, by.y = commonvar)
    
      if (Rshiny==FALSE){
        if (mapScenarios==FALSE){
      # Create and output maps
      filename <- paste(path_results,"/maps/",file_sum,"_prediction_stream_maps.pdf",sep="") 
        }else{
          filename <- paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,"_prediction_stream_maps.pdf",sep="")
          
        }

      pdf(file=filename)
      }
# loop through each of the variables...
      for (k in 1:length(master_map_list)) {
        testNAvar<-eval(parse(text = paste0("testNA$",master_map_list[k])))
        if (mapScenarios==FALSE){
          if (testNAvar==0){
            Mcolors <- predictionMapColors  
          }else{
            Mcolors <- c("gray",predictionMapColors)}
        }else{
          Mcolors <- scenarioMapColors
        }
        
        if (input$batch=="Batch" & Rshiny==TRUE){
          if (mapScenarios==FALSE){
          filename<- paste(path_results,"/maps/Interactive/Stream/",file_sum,"_",master_map_list[k],".pdf",sep="")
          }else{
            if (!dir.exists(paste(path_results,"/scenarios/",input$scenarioName,sep=""))){
              dir.create(paste(path_results,"/scenarios/",input$scenarioName,sep=""),showWarnings = FALSE)
            }
            if (!dir.exists(paste(path_results,"/scenarios/",input$scenarioName,"/Stream/",sep=""))){
              dir.create(paste(path_results,"/scenarios/",input$scenarioName,"/Stream/",sep=""),showWarnings = FALSE)
            }
            filename<- paste(path_results,"/scenarios/",input$scenarioName,"/Stream/",
                             input$scenarioName,"_",file_sum,"_",scenario_map_list[k],".pdf",sep="")
          }
          pdf(filename)
        }

        if (existGeoLines==TRUE){
          plot(GeoLines,col=1,lwd=0.1,xlim=lon_limit,ylim=lat_limit,bg = predictionMapBackground)
        }
        
        # obtain variable settings
        mapvarname <- paste("lineShape$MAPCOLORS",k,sep="")
        # select the shading colors for a given mapping variable
        if (existGeoLines==TRUE){
          xtext <- paste("sp::plot(lineShape,col=",mapvarname,",lwd=lineWidth, add=TRUE)",sep="")
          eval(parse(text=xtext))
        } else {
          xtext <- paste("sp::plot(lineShape,col=",mapvarname,",lwd=lineWidth,bg = predictionMapBackground))",sep="")
          eval(parse(text=xtext))
        }
        if (mapScenarios==FALSE){
        title(master_map_list[k],cex.main = predictionTitleSize)
        }else{
          if (Rshiny==FALSE){
            title(paste(scenario_name,scenario_map_list[k],sep=" "))  
          }else{
            title(paste(input$scenarioName,master_map_list[k],sep=" "),cex.main = predictionTitleSize)  
          }
        }


        
        legend("bottomleft",break1[k][[1]],lty=nlty,cex=predictionLegendSize,title=mapunits.list[k],
               bg=predictionLegendBackground,lwd=nlwd, col=Mcolors[1:length(break1[k][[1]])], bty="o")
        
        if (input$batch=="Batch" & Rshiny==TRUE){
          dev.off()
          if (k==length(master_map_list)){
            if (regexpr("catchment",paste(output_map_type,collapse=","))<0){
            shell.exec(filename)
            }
          }
        }
        }#end variable loop
      
      if (Rshiny==FALSE){
        dev.off()  # shuts down current graphics device
      graphics.off()  # shuts down all open graphics devices    
      }
      
      #output shapefile
      if (outputERSImaps[1]=="yes"){
      lineShape <- sp::merge(lineShape, dmapAll, by.x = commonvar, by.y = commonvar)
      lineShape<-lineShape[,which(regexpr("MAPCOLORS",names(lineShape))<0)]
      
      if (Rshiny==FALSE){
      if (!dir.exists(paste(path_results,"maps/ESRI_ShapeFiles/",sep=""))){
        dir.create(paste(path_results,"maps/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
      }
      if (!dir.exists(paste(path_results,"maps/ESRI_ShapeFiles/prediction/",sep=""))){
        dir.create(paste(path_results,"maps/ESRI_ShapeFiles/prediction/",sep=""),showWarnings = FALSE)
      }

      writeSpatialShape(lineShape,paste(path_results,"/maps/ESRI_ShapeFiles/prediction/lineShape",sep=""))
      cat(showWKT(proj4string(lineShape)),file=paste(path_results,"/maps/ESRI_ShapeFiles/prediction/lineShape.prj",sep="")) 
      
      }else if (mapScenarios==TRUE & input$batch=="Batch"){
        if (!dir.exists(paste(path_results,"scenarios/",scenario_name,"/ESRI_ShapeFiles/",sep=""))){
          dir.create(paste(path_results,"scenarios/",scenario_name,"/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
        }
      
        maptools::writeSpatialShape(lineShape,paste(path_results,"/scenarios/",scenario_name,"/ESRI_ShapeFiles/lineShape",sep=""))
        cat(rgdal::showWKT(sp::proj4string(lineShape)),file=paste(path_results,"/scenarios/",scenario_name,"/ESRI_ShapeFiles/lineShape.prj",sep="")) 

              }else if (input$batch=="Batch"){
        if (!dir.exists(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/",sep=""))){
          dir.create(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
        }
        if (!dir.exists(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/prediction/",sep=""))){
          dir.create(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/prediction/",sep=""),showWarnings = FALSE)
        }
        
                maptools::writeSpatialShape(lineShape,paste(path_results,"/maps/Interactive/ESRI_ShapeFiles/prediction/lineShape",sep=""))
                cat(rgdal::showWKT(sp::proj4string(lineShape)),file=paste(path_results,"/maps/Interactive/ESRI_ShapeFiles/prediction/lineShape.prj",sep="")) 
      }
      }

      
      }

      if (((paste(output_map_type,collapse="") %in% c("catchment","both") & Rshiny==FALSE) | 
          (Rshiny==TRUE & input$mapType=="Catchment" & mapScenarios==FALSE) |
          (Rshiny==TRUE & regexpr("catchment",paste(output_map_type,collapse=","))>0  & mapScenarios==TRUE)) & existpolyShape==TRUE) {
        commonvar <- polyWaterid
        names(dmapfinal)[1]<-commonvar
        names(dmapAll)[1]<-commonvar
        # merge selected variables to the shape file
        polyShape <- sp::merge(polyShape, dmapfinal, by.x = commonvar, by.y = commonvar)
        
        if (Rshiny==FALSE){
        # Create and output maps
          if (mapScenarios==FALSE){
        filename <- paste(path_results,"/maps/",file_sum,"_prediction_catchment_maps.pdf",sep="")
          }else{
            filename <- paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,"_prediction_catchment_maps.pdf",sep="")
          }
        pdf(file=filename)
        }
        
        # loop through each of the variables...
        for (k in 1:length(master_map_list)) {
  
          
          if (input$batch=="Batch"){
            if (mapScenarios==FALSE){
            filename<- paste(path_results,"/maps/Interactive/Catchment/",file_sum,"_",master_map_list[k],".pdf",sep="")
            }else{
              if (!dir.exists(paste(path_results,"/scenarios/",input$scenarioName,sep=""))){
                dir.create(paste(path_results,"/scenarios/",input$scenarioName,sep=""),showWarnings = FALSE)
              }
              if (!dir.exists(paste(path_results,"/scenarios/",input$scenarioName,"/Catchment/",sep=""))){
                dir.create(paste(path_results,"/scenarios/",input$scenarioName,"/Catchment/",sep=""),showWarnings = FALSE)
              }
              filename<- paste(path_results,"/scenarios/",input$scenarioName,"/Catchment/",
                               input$scenarioName,"_",file_sum,"_",scenario_map_list[k],".pdf",sep="")
            }
            
            pdf(filename)
          }
          if (existGeoLines==TRUE){
            plot(GeoLines,col=1,lwd=0.1,xlim=lon_limit,ylim=lat_limit,bg = predictionMapBackground)
          }
          
          # obtain variable settings
          mapvarname <- paste("polyShape$MAPCOLORS",k,sep="")
          
          # select the shading colors for a given mapping variable
          if (existGeoLines==TRUE){
            xtext <- paste("sp::plot(polyShape,col=",mapvarname,",lwd=0.01, lty=0, add=TRUE)",sep="")
            eval(parse(text=xtext))
          } else {
            xtext <- paste("sp::plot(polyShape,col=",mapvarname,",lwd=0.01, lty=0,bg = predictionMapBackground)",sep="")
            eval(parse(text=xtext))
          }
          
          if (mapScenarios==FALSE){
          title(master_map_list[k],cex.main = predictionTitleSize)
          }else{
            if (Rshiny==FALSE){
            title(paste(scenario_name,scenario_map_list[k],sep=" "))  
            }else{
              title(paste(input$scenarioName,scenario_map_list[k],sep=" "),cex.main = predictionTitleSize)  
            }
            
            
          }


          
          legend("bottomleft",break1[k][[1]],lty=nlty,cex=predictionLegendSize,title=mapunits.list[k],
                 bg=predictionLegendBackground,lwd=nlwd, col=Mcolors[1:length(break1[k][[1]])], bty="o")
          if (input$batch=="Batch"){
            dev.off()
            if (k==length(master_map_list)){
              shell.exec(filename)
            }
          }
          
        }#end variable loop
        
        if (Rshiny==FALSE){
        dev.off()  # shuts down current graphics device
        graphics.off()  # shuts down all open graphics devices
        }
        
        #output shapefile
        if (outputERSImaps[2]=="yes"){
          polyShape <- sp::merge(polyShape, dmapAll, by.x = commonvar, by.y = commonvar)
          polyShape<-polyShape[,which(regexpr("MAPCOLORS",names(polyShape))<0)]
          
          if (Rshiny==FALSE){
          if (!dir.exists(paste(path_results,"maps/ESRI_ShapeFiles/",sep=""))){
            dir.create(paste(path_results,"maps/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
          }
          if (!dir.exists(paste(path_results,"maps/ESRI_ShapeFiles/prediction/",sep=""))){
            dir.create(paste(path_results,"maps/ESRI_ShapeFiles/prediction/",sep=""),showWarnings = FALSE)
          }
          

          writeSpatialShape(polyShape,paste(path_results,"/maps/ESRI_ShapeFiles/prediction/polyShape",sep=""))
          cat(showWKT(proj4string(polyShape)),file=paste(path_results,"/maps/ESRI_ShapeFiles/prediction/polyShape.prj",sep="")) 
          
          
        }else if (mapScenarios==TRUE  & input$batch=="Batch"){
          if (!dir.exists(paste(path_results,"scenarios/",scenario_name,"/ESRI_ShapeFiles/",sep=""))){
            dir.create(paste(path_results,"scenarios/",scenario_name,"/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
          }
          
          maptools::writeSpatialShape(polyShape,paste(path_results,"/scenarios/",scenario_name,"/ESRI_ShapeFiles/polyShape",sep=""))
          cat(rgdal::showWKT(sp::proj4string(polyShape)),file=paste(path_results,"/scenarios/",scenario_name,"/ESRI_ShapeFiles/polyShape.prj",sep="")) 
          
        }else if (input$batch=="Batch"){
          if (!dir.exists(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/",sep=""))){
            dir.create(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
          }
          if (!dir.exists(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/prediction/",sep=""))){
            dir.create(paste(path_results,"maps/Interactive/ESRI_ShapeFiles/prediction/",sep=""),showWarnings = FALSE)
          }
          
          maptools::writeSpatialShape(polyShape,paste(path_results,"/maps/Interactive/ESRI_ShapeFiles/prediction/polyShape",sep=""))
          cat(rgdal::showWKT(sp::proj4string(polyShape)),file=paste(path_results,"/maps/Interactive/ESRI_ShapeFiles/prediction/polyShape.prj",sep="")) 
        }
        }
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
      }#end if file.exists(predictList) or if_predict="yes"
      
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("predictMaps.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function
