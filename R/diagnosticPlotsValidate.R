#'@title diagnosticPlotsValidate
#'@description Creates diagnostic plots for validation sites output to ~/estimate/(run_id)_validation_plots.pdf.pdf
#'Uses subroutines: checkBinaryMaps, diagnosticMaps, errorOccurred.
#'@param path_results path to results directory
#'@param file_sum user specified run_id 
#'@param classvar character vector of user specified spatially contiguous discrete classification variables from sparrow_control
#'@param class_landuse character vector of class_landuses from sparrow_control
#'@param vsitedata.demtarea.class Total drainage area classification variable for validation sites. Calculated by `calcDemtareaClass(vsitedata$demtarea,batch_mode,ErrorOccured)`
#'@param vsitedata sitedata for validation. Calculated by `subdata[(subdata$vdepvar > 0), ]`
#'@param sitedata.landuse Land use for incremental basins for diagnostics.  Calculated by `calcIncremLandUse(subdata,class_landuse,subdata$staidseq,minimum_reaches_separating_sites,batch_mode,ErrorOccured)`
#'@param estimate.list Named list output of estimate.R subroutine containing objects: sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list, vANOVA.list,vMdiagnostics.list
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit,lon_limit,master_map_list,
#'lineShapeName,lineWaterid,polyShapeName,ployWaterid,LineShapeGeo, LineShapeGeo,CRStext,convertShapeToBinary.list,
#'map_siteAttributes.list,residual_map_breakpoints,site_mapPointScale, if_verify_demtarea_maps 
#'@param add_vars
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#
# diagnosticPlotsValidate.R
#
#############################################################################
# Create diagnostic plots for validation sites

diagnosticPlotsValidate <- function(path_results,file_sum,classvar,class_landuse,vsitedata.demtarea.class,
                               vsitedata,sitedata.landuse,estimate.list,mapping.input.list,add_vars,
                               batch_mode,ErrorOccured) {
  
  # INPUT objects:
  # sitedata
  # sitedata.landuse
  # estimate.list
  # 
  
  #########################
  # Create Global Variables
  #########################
  if (ErrorOccured=="no"){
    tryIt<-try({

  # create global variable from list names (mapping.input.list)
  for(i in 1:length(mapping.input.list)){
    tempobj=mapping.input.list[[i]]
    eval(parse(text=paste(names(mapping.input.list)[[i]],"= tempobj")))
  }
  
  # create global variable from list names (Mdiagnostics.list)
  for(i in 1:length(estimate.list$vMdiagnostics.list)){
    tempobj=estimate.list$vMdiagnostics.list[[i]]
    eval(parse(text=paste(names(estimate.list$vMdiagnostics.list)[[i]],"= tempobj")))
  }

  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(vsitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(vsitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("vsitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  } 

  # Create 'classvar2' for plotting landuse non-contiguous class
  #   following code executes:  classvar2 <- c("forest_pct","agric_pct","urban_pct","shrubgrass_pct")
  if(!is.na( class_landuse[1])){
    classvar2 <- character(length(class_landuse))
    for (i in 1:length(class_landuse)) {
      classvar2[i] <- paste(class_landuse[i],"_pct",sep="")
    }
  }
  
  
  filename <- paste(path_results,"/estimate/",file_sum,"_validation_plots.pdf",sep="")
  pdf(file=filename,font="Helvetica")
  
  
  
  ##################################################

  # Diagnostics by CLASS (contiguous geographic units)

  plotmrb <- function(xmrb,class,Obs,predict,yldobs,yldpredict,Resids) {
   xmrb <- as.double(xmrb)
   # observed vs predicted 
   par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex=diagnosticPlotPointSize)  # 4 plots on one page

   #  observed vs. predicted mass
   df <- data.frame(predict,Obs)
   df <- subset(df,class == xmrb)
   nsites <- as.numeric(length(df$predict))

   if(nsites > 0) {
    plot(df$predict,df$Obs,log="xy",pch=1,
         ylab=paste0("OBSERVED LOAD (",loadUnits,")"),xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
    title(font.main=2,main=bquote(paste("Observed vs Predicted Load \nCLASS Region = ",.(xmrb),"(n=",.(nsites),")") ))
    lines(df$Obs,df$Obs, col=2)

   # observed vs. predicted yield
   df <- data.frame(yldpredict,yldobs)
   df <- subset(df,class == xmrb)
   plot(df$yldpredict,df$yldobs,log="xy",pch=1,main="Observed vs Predicted \nYield",
        ylab=paste0("OBSERVED YIELD (",yieldUnits,")"),xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
   lines(df$yldobs,df$yldobs, col=2)

   # mass residual plot
   df <- data.frame(predict,Resids)
   df <- subset(df,class == xmrb)
   plot(df$predict,df$Resids,log="x",pch=1,main="Residuals vs Predicted \nLoad",
     ylab="LOG RESIDUAL",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
   eq <- rep(0,length(df$predict))
   lines(df$predict,eq, col=2)

   # yield residual plot
   df <- data.frame(yldpredict,Resids)
   df <- subset(df,class == xmrb)
   plot(df$yldpredict,df$Resids,log="x",pch=1,main="Residuals vs Predicted \nYield",
     ylab="LOG RESIDUAL",xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
   eq <- rep(0,length(df$Resids))
   lines(df$yldpredict,eq, col=2)
   }
  } # end function

  # Obtain CLASS region numbers
  grp <- table(class[,1])   # get labels
  xx <- as.data.frame(grp)  # convert table to dataframe...
  grp <- as.numeric(levels(xx$Var1)[xx$Var1])  # convert factor levels to numeric values

  ##################################################
  # PERFORMANCE METRICS FOR NO MONITORING ADJUSTMENT
  ##################################################

  # Full spatial domain

  # observed vs predicted 
  par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex=diagnosticPlotPointSize)  # 4 plots on one page

  # observed vs. predicted mass
  plot(ppredict,Obs,log="xy",pch=1,main="MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Load",
       ylab=paste0("OBSERVED LOAD (",loadUnits,")"),xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  lines(Obs,Obs, col=2)

  # observed vs. predicted yield
  plot(pyldpredict,pyldobs,log="xy",pch=1,main="MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Yield",
       ylab=paste0("OBSERVED YIELD (",yieldUnits,")"),xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
  lines(pyldobs,pyldobs, col=2)

  # mass residual plot
  plot(ppredict,pResids,log="x",pch=1,main="Residuals vs Predicted \nLoad",
   ylab="LOG RESIDUAL",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  abline(h=0,col=2)

  # yield residual plot
  plot(pyldpredict,pResids,log="x",pch=1,main="Residuals vs Predicted \nYield",
    ylab="LOG RESIDUAL",xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
  abline(h=0,col=2)


  par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex=diagnosticPlotPointSize)  # 4 plots on one page

  # Residual plots
   boxplot(pResids,ylab="LOG RESIDUAL",main="MODEL SIMULATION PERFORMANCE \nResiduals")

  # Obs-Pred ratio boxplot
   boxplot(pratio.obs.pred,ylim = c(0,2),ylab="RATIO OBSERVED TO PREDICTED",
     main="MODEL SIMULATION PERFORMANCE \nObserved / Predicted Ratio")

  # Normality probability plot
   qqnorm(pResids,ylab="Log Residuals")
   qqline(pResids, col = 2)

  # Squared residuals vs predicted
  Resids2 <- pResids**2
  plot(ppredict,Resids2,log="xy",pch=1,main="Squared Residuals vs Predicted Load",
    ylab="SQUARED LOG RESIDUALS",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  lwresids <- lowess(ppredict,Resids2, f = 0.5, iter = 3)
  lwy <- lwresids$y
  lwx <- lwresids$x
  lines(lwx,lwy,col=2)

  ##########################
  # Diagnostics for Ratio by class (one plot per page)
 
  # sitedata.demtarea.class regions
  for (k in 1:length(classvar)) {
    par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex=diagnosticPlotPointSize)  # 1 plots on one page
    vvar <- vsitedata.demtarea.class
    boxplot(pratio.obs.pred ~ vvar,log="y",main="Ratio Observed to Predicted by Deciles",
            xlab="Upper Bound for Total Drainage Area Deciles (km2)",ylab="Observed to Predicted Ratio")
    abline(h = 1, col = "red", lwd = 1) 
  }

  # "classvar" regions
  for (k in 1:length(classvar)) {
   par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex=diagnosticPlotPointSize)  # 1 plots on one page
   vvar <- as.numeric(eval(parse(text=paste("vsitedata$",classvar[k],sep="") )))
   boxplot(pratio.obs.pred ~ vvar,log="y",main="Ratio Observed to Predicted",
           xlab=classvar[k],ylab="Observed to Predicted Ratio")
   abline(h = 1, col = "red", lwd = 1) 
  }
 
  # 'classvar2" decile boxplots
  if(!is.na( class_landuse[1])){
    for (k in 1:length(classvar2)) {
     par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex=diagnosticPlotPointSize)  # 1 plots on one page
     vvar <- as.numeric(eval(parse(text=paste("vsitedata.landuse$",classvar2[k],sep="") )))
     iprob<-10
     chk <- unique(quantile(vvar, probs=0:iprob/iprob))
     chk1 <- 11 - length(chk)
     if(chk1 == 0) {
       qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE)) 
       avars <- quantile(vvar, probs=0:iprob/iprob)
       qvars2 <- numeric(length(qvars))
       for (j in 1:10) {
         for (i in 1:length(qvars)){
           if(qvars[i] == j) {
             qvars2[i] <- round(avars[j+1],digits=0)
           }
         }
       }
       xxlab <- paste("Upper Bound for ",classvar2[k],sep="")  
       boxplot(pratio.obs.pred ~ qvars2,log="y",main="Ratio Observed to Predicted by Deciles",
               xlab=xxlab,ylab="Observed to Predicted Ratio")
       abline(h = 1, col = "red", lwd = 1) 
     } else {  # non-unique classes
       plot(vvar,pratio.obs.pred,log="y",main="Ratio Observed to Predicted",
          xlab=classvar2[k],ylab="Observed to Predicted Ratio")
       abline(h = 1, col = "red", lwd = 1) 
     }
    } # end 'classvar2' loop
  }

  ##################################################
  # Diagnostics by CLASS (contiguous geographic units)

  for (i in 1:length(grp)) {
    plotmrb(grp[i],class,Obs,ppredict,pyldobs,pyldpredict,pResids)
  }


  #################
  # Residual MAPS
  #################

  # Setup GEOLINES basemap, if available

 #if(!is.na(LineShapeGeo)) {
  existGeoLines<-checkBinaryMaps("LineShapeGeo", LineShapeGeo,"GeoLines", path_gis,batch_mode,ErrorOccured)
  if(existGeoLines==TRUE) { 
    
   #LineGeo <- readOGR(dsn=path_gis, layer=LineShapeGeo)
   #LineGeo <- spTransform(LineGeo, CRS(CRStext))
   #GeoLines <- as(LineGeo, "SpatialLinesDataFrame")
   
  Resids <- pResids
  ratio.obs.pred <- pratio.obs.pred
  
  # Map residuals (8 classes)

  mapdata <- data.frame(xlat,xlon,Resids,ratio.obs.pred)
  diagnosticMaps("Resids",mapdata,GeoLines,lat_limit,lon_limit,
                 c("threshold","all"),"Log Residuals",mapping.input.list,batch_mode,ErrorOccured)

  ##########################
  # Map Ratios observed to predicted 
  diagnosticMaps("ratio.obs.pred",mapdata,GeoLines,lat_limit,lon_limit,
                 c("threshold","all"),"Obs/Pred Ratio",mapping.input.list,batch_mode,ErrorOccured)

  #output residuals shapefile
  if (outputERSImaps[3]=="yes"){
    Obsyield <- Obs / vsitedata$demtarea
    predictYield <- ppredict / vsitedata$demtarea
    origWaterid<-vsitedata$waterid_for_RSPARROW_mapping
    
    dd <- data.frame(vsitedata,origWaterid,Obs,ppredict,Obsyield,predictYield,pResids,pratio.obs.pred,xlat,xlon)
    
    keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
               "ppredict","Obsyield","predictYield","pResids","pratio.obs.pred","xlat","xlon")

    validationResidShape <- dd[keeps]
    
    if (length(na.omit(add_vars))!=0){
      add_data<-data.frame(vsitedata[,which(names(vsitedata) %in% add_vars)])
      if (length(add_vars)==1){
        names(add_data)<-add_vars
      }
      validationResidShape<-cbind(validationResidShape,add_data)
    }
    
    validationResidShape <-SpatialPointsDataFrame(validationResidShape[,c("xlon","xlat")],validationResidShape[,which(!names(validationResidShape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
    
    if (!dir.exists(paste(path_results,"maps/ESRI_ShapeFiles/",sep=""))){
      dir.create(paste(path_results,"maps/ESRI_ShapeFiles/",sep=""),showWarnings = FALSE)
    }
    if (!dir.exists(paste(path_results,"maps/ESRI_ShapeFiles/residuals/",sep=""))){
      dir.create(paste(path_results,"maps/ESRI_ShapeFiles/residuals/",sep=""),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(validationResidShape,paste(path_results,"maps/ESRI_ShapeFiles/residuals/validationResidShape",sep=""))
    cat(showWKT(proj4string(validationResidShape)),file=paste(path_results,"/maps/ESRI_ShapeFiles/residuals/validationResidShape.prj",sep="")) 
    
  }

 }  # end check for existence of line map

  dev.off()  # shuts down current graphics device
  graphics.off()  # shuts down all open graphics devices

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("diagnosticPlotsValidate.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function
