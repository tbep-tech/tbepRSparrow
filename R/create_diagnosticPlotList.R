create_diagnosticPlotList<-function(){
  plotList<-list()
  plotList$p1<-list()
  plotList$p1$title<-"ModEstimation_Obs_v_Pred"
  plotList$p1$header<-"# Model Estimation Performance Diagnostics
Diagnostics are based on the use of conditioned (monitoring-adjusted) predictions. These predictions provide the most accurate reach predictions for use in calibrating the model. The associated residuals and observed to predicted ratios shown in the following section provide the most relevant measures of the accuracy of the model fit to observed loads.

## Observed vs. predicted for loads and yields and log residuals vs. predicted loads and yields\n"
  plotList$p1$plotParams<-"diagnosticPlotPointStyle,diagnosticPlotPointSize,
                                     predict,Obs,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,yldpredict,yldobs,yieldUnits,Resids,hline,validation"
plotList$p1$plotFunc<-function(plotParams.list){
  unPackList(lists = list(plotParams.list = plotParams.list),
             parentObj = list(NA))
  assign("validation",validation,env = parent.frame())
  
   if (!validation){ 
     
     plotObs<-Obs
     plotpredict<-predict
     plotyldobs<-yldobs
     plotyldpredict<-yldpredict
     plotResids<-Resids
     
     p<-diagnoticPlots_4panel_A(plotpredict,plotObs,plotyldpredict,plotyldobs,sitedata,plotResids,plotclass=NA,
                                plotTitles = c("'MODEL ESTIMATION PERFORMANCE \n(Monitoring-Adjusted Predictions) \nObserved vs    Predicted Load'",
                                               "'MODEL ESTIMATION PERFORMANCE \nObserved vs Predicted Yield'",
                                               "'Residuals vs Predicted \nLoad'",
                                               "'Residuals vs Predicted \nYield'"),
                                loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                pnch,markerCols,hline,filterClass = NA)

  return(p)
  }
}#end plotIndex_1$plotFunc  

  plotList$p2<-list()
  plotList$p2$title<-"ModEstimation_Box_and_Quantile_Resid"
  plotList$p2$header<-"# Model Estimation Performance Diagnostics
Diagnostics are based on the use of conditioned (monitoring-adjusted) predictions. These predictions provide the most accurate reach predictions for use in calibrating the model. The associated residuals and observed to predicted ratios shown in the following section provide the most relevant measures of the accuracy of the model fit to observed loads.

## Boxplots of residuals and observed/predicted ratios, normal quantile plot of standardized residuals, and plot of squared residuals vs. predicted loads\n"  
  plotList$p2$plotParams<-"Resids,ratio.obs.pred,standardResids,
                                     predict,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,validation"
  plotList$p2$plotFunc<-function(plotParams.list){
    
      unPackList(lists = list(plotParams.list = plotParams.list),
                 parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
     if (!validation){  
       plotResids<-Resids
       plot.ratio.obs.pred<-ratio.obs.pred
       plot.standardResids<-standardResids
       plotpredict<-predict
       
      p<-diagnoticPlots_4panel_B(sitedata,plotResids,plot.ratio.obs.pred,plot.standardResids,plotpredict,
                                         plotTitles = c("'MODEL ESTIMATION PERFORMANCE \nResiduals'",
                                                        "'MODEL ESTIMATION PERFORMANCE \nObserved / Predicted Ratio'",
                                                        "'Normal Q-Q Plot'",
                                                        "'Squared Residuals vs Predicted Load'"),
                                         loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                         pnch,markerCols,hline)
        assign("Resids2",Resids2,env = parent.frame())

      return(p)
    }
  }#end p2 func
  
  
  plotList$p3<-list()
  plotList$p3$title<-"ModEstimation_Conditioned_v_Unconditioned_loads"
  plotList$p3$header<-"# Model Estimation Performance Diagnostics
Diagnostics are based on the use of conditioned (monitoring-adjusted) predictions. These predictions provide the most accurate reach predictions for use in calibrating the model. The associated residuals and observed to predicted ratios shown in the following section provide the most relevant measures of the accuracy of the model fit to observed loads.

## Conditioned prediction loads vs. unconditioned (simulated) prediction loads\n"
  
  plotList$p3$plotParams<-"ppredict,
                                     predict,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,validation"
  plotList$p3$plotFunc<-function(plotParams.list){
    
      unPackList(lists = list(plotParams.list = plotParams.list),
                 parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
       if (!validation){
      markerText<-"~paste('</br> Simulated Load: ',ppredict,
                   '</br> Predicted Load: ',predict"
      data<-data.frame(ppredict = ppredict,predict = predict)
      
      markerText<-addMarkerText(markerText,add_plotlyVars,data, sitedata)$markerText
      data<-addMarkerText(markerText,add_plotlyVars, data,sitedata)$mapData
      
      p <- plotlyLayout(ppredict,predict, log = "xy", nTicks = 5, digits = 0,
                        xTitle = paste0("Simulated Load (",loadUnits,")"), xZeroLine = FALSE,
                        yTitle = paste0("Monitoring-Adjusted Load (",loadUnits,")"), yZeroLine = FALSE,
                        plotTitle = "Monitoring-Adjusted vs. Simulated Loads",
                        legend = FALSE,showPlotGrid = showPlotGrid)
      p <- p %>% add_trace(data = data, x = ~ppredict, y = ~predict, 
                           type = "scatter", 
                           mode = "markers",
                           marker = eval(parse(text = markerList)),
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
      p <- p %>% add_trace(x = ppredict, y = ppredict, 
                           type = "scatter", 
                           mode = "lines",
                           color = I("red"),
                           hoverinfo = 'text',
                           text = "Simulated Load")
      return(p)
    }
  }#end p3 func
  ##############################################
  plotList$p4<-list()
  plotList$p4$title<-"ModEstimation_Correlations_ExplanitoryVariables"
  plotList$p4$header<-  "# Observed to predicted ratio vs. the area-weighted mean values of the user-selected explanatory variables for the incremental areas between calibration sites \n
Output only if control setting if_corrExplanVars<-'yes' selected and a value of 1 entered for 'parmCorrGroup' column in the 'parameters.csv' file.
"
  
  plotList$p4$plotParams<-"i,Cor.ExplanVars.list,corrData,
                            markerList,add_plotlyVars,sitedata,showPlotGrid,
                                     pnch,markerCols,validation"
  plotList$p4$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    if (!validation){
    
    if(min(Cor.ExplanVars.list$cmatrixM_all[,i])<0 | max(Cor.ExplanVars.list$cmatrixM_all[,i])<0) {
      logStr<-""
    }else{
      logStr<-"x"
    }
    
    markerText<-"~paste('</br>',Cor.ExplanVars.list$names[i],': ',xvar,
                   '</br> RATIO OBSERVED TO PREDICTED: ',corrData"
    data<-data.frame(xvar = Cor.ExplanVars.list$cmatrixM_all[,i],corrData = corrData)
    
    markerText<-addMarkerText(markerText,add_plotlyVars,data, sitedata)$markerText
    data<-addMarkerText(markerText,add_plotlyVars, data,sitedata)$mapData
    
    p<-plotlyLayout( Cor.ExplanVars.list$cmatrixM_all[,i],corrData, log = logStr, nTicks = 5, digits = 0,
                  xTitle = paste0("AREA-WEIGHTED EXPLANATORY VARIABLE (",Cor.ExplanVars.list$names[i],")"), 
                  xZeroLine = FALSE,
                  yTitle = "RATIO OBSERVED TO PREDICTED", yZeroLine = FALSE,
                  plotTitle = paste("Observed to Predicted Ratio vs Area-Weighted Explanatory Variable \nFor Incremental Areas between Calibration Sites; Variable Name = ",Cor.ExplanVars.list$names[i]) ,
                  legend = FALSE,showPlotGrid = showPlotGrid) %>%
     add_trace(data = data, x = ~xvar, y = ~corrData, 
                type = "scatter", 
                mode = "markers",
                marker = eval(parse(text = markerList)),
                hoverinfo = 'text',
                text = eval(parse(text = markerText)))
    
    return(p)
    }
  }#end p4 func
  ##################################################
  plotList$p5<-list()
  plotList$p5$title<-"ModEstimation_DrainageArea_DecileClass_Box"
  plotList$p5$header<-"## Boxplots of the observed to predicted loads vs. the decile classes of the total drainage area for the calibration sites\n"  
  plotList$p5$plotParams<-"sitedata.demtarea.class,ratio.obs.pred,showPlotGrid,hline,validation"
  plotList$p5$plotFunc<-function(plotParams.list){
    
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    if (!validation){ 
 
      # sitedata.demtarea.class regions
      
      vvar <- sitedata.demtarea.class
      
      p <- plotlyLayout(NA,ratio.obs.pred, log = "y", nTicks = 7, digits = 0,
                        xTitle = "Upper Bound for Total Drainage Area Deciles (km2)",  
                        xZeroLine = FALSE,xLabs = sort(as.numeric(unique(vvar))),
                        yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
                        plotTitle = "Ratio Observed to Predicted by Deciles",
                        legend = FALSE,showPlotGrid = showPlotGrid)
      p <- p %>% add_trace(y = ratio.obs.pred,x = vvar, type = 'box', color = I("black"), 
                           fillcolor = "white")
      p <- p %>% layout(shapes = list(hline(1)))
      return(p)
    }
    }#p5 func
  
  ############################################
  plotList$p6<-list()
  plotList$p6$title<-"ModEstimation_Classvar_Decile_Box"
  plotList$p6$header<-  "## Boxplots of the observed to predicted loads vs. the contiguous spatial classes specified by users in the 'classvar' control setting (e.g., HUC-4) \n"
  
  plotList$p6$plotParams<-"k,sitedata,classvar,boxvar,showPlotGrid,hline,validation"
  
  plotList$p6$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    if (!validation){
  
  vvar <- as.numeric(eval(parse(text=paste0("sitedata$",classvar[k]) )))
  
  p<-plotlyLayout(NA,boxvar, log = "y", nTicks = 7, digits = 0,
               xTitle = classvar[k],  
               xZeroLine = FALSE,xLabs = sort(as.numeric(unique(vvar))),
               yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
               plotTitle = "Ratio Observed to Predicted",
               legend = FALSE,showPlotGrid = showPlotGrid) %>%
    add_trace(y = boxvar,x = vvar, type = 'box', color = I("black"), 
              fillcolor = "white") %>%
    layout(shapes = list(hline(1)))
  return(p)
    }
  }#p6 func
  
  
  ###############################################
  plotList$p7<-list()
  plotList$p7$title<-"ModEstimation_LanduseClass_Decile_Box"
  plotList$p7$header<-  "## Boxplots of the observed to predicted loads vs. the deciles of the land-use class variable specified by users in the 'class_landuse' control setting\n
The land-use classes expressed as a percentage of the incremental drainage area extending from the calibration site to the nearest upstream site locations
"
  
  plotList$p7$plotParams<-"k,sitedata.landuse,classvar2,boxvar,showPlotGrid,hline,validation"
  
  plotList$p7$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    if (!validation){
      vvar <- as.numeric(eval(parse(text=paste0("sitedata.landuse$",classvar2[k]) )))
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
        xxlab <- paste0("Upper Bound for ",classvar2[k])  
        
        p<-plotlyLayout(NA,boxvar, log = "y", nTicks = 7, digits = 0,
                     xTitle = xxlab,  
                     xZeroLine = FALSE,xLabs = sort(as.numeric(unique(qvars2))),
                     yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
                     plotTitle = "Ratio Observed to Predicted by Deciles",
                     legend = FALSE,showPlotGrid = showPlotGrid) %>%
          add_trace(y = boxvar,x = qvars2, type = 'box', color = I("black"), 
                    fillcolor = "white") %>%
          layout(shapes = list(hline(1)))
        
        
      } else {  # non-unique classes
        
        p<-plotlyLayout(NA,boxvar, log = "y", nTicks = 7, digits = 0,
                     xTitle = classvar2[k],  
                     xZeroLine = FALSE,xLabs = sort(as.numeric(unique(vvar))),
                     yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
                     plotTitle = "Ratio Observed to Predicted",
                     legend = FALSE,showPlotGrid = showPlotGrid) %>%
          add_trace(y = boxvar,x = vvar, type = 'box', color = I("black"), 
                    fillcolor = "white") %>%
          layout(shapes = list(hline(1)))
   
      }
      return(p)
    }
    
  }#p7 func
 
  ###############################################
  plotList$p8<-list()
  plotList$p8$title<-"ModEstimation_4panel_Classvar"
  plotList$p8$header<-  "## Four-plot panels reported separately for each of the contiguous spatial classes specified for the first variable entry for the 'classvar[1]' control setting\n
The panels include:  observed vs. predicted loads, observed vs. predicted yields, log residuals vs. predicted loads, and log residuals vs. predicted yields 
"
  
  plotList$p8$plotParams<-"i,grp,class,xx,Obs,predict,yldobs,yldpredict,Resids,add_plotlyVars,showPlotGrid,
                                     markerList,pnch,markerCols,sitedata,hline,validation"
  
  plotList$p8$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
      
       if (!validation){

      plotclass<-class[,1]
      plotObs<-Obs
      plotpredict<-predict
      plotyldobs<-yldobs
      plotyldpredict<-yldpredict
      plotResids<-Resids
      
      
      xmrb<-grp[i]
      
      xmrb <- as.double(xmrb)
      
     p<-diagnoticPlots_4panel_A(plotpredict,plotObs,plotyldpredict,plotyldobs,sitedata,plotResids,plotclass,
                                          plotTitles = c("paste0('Observed vs Predicted Load \nCLASS Region = ',filterClass,'(n=',nsites,')')",
                                                         "'Observed vs Predicted \nYield'",
                                                         "'Residuals vs Predicted \nLoad'",
                                                         "'Residuals vs Predicted \nYield'"),
                                loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                          pnch,markerCols,hline,filterClass = xmrb)
     return(p)
      
      
    }
  }#p8 func
 #############################################
  plotList$p9<-list()
  plotList$p9$title<-"ModSimulation_Obs_v_Pred"
  plotList$p9$header<-"# Model Simulation Performance Diagnostics
Diagnostics are based on the use of unconditioned predictions (i.e., predictions that are not adjusted for monitoring loads). These predictions (and the associated residuals and observed to predicted ratios shown in the following section) provide the best measure of the predictive skill of the estimated model in simulation mode. The simulated predictions are computed using mean coefficients from the NLLS model estimated with monitoring-adjusted (conditioned) predictions. \n
Four-plot panel for observed vs. predicted for loads and yields, and log residuals vs. predicted loads and yields"
  
  plotList$p9$plotParams<-"diagnosticPlotPointStyle,diagnosticPlotPointSize,
                                     ppredict,Obs,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,pyldpredict,pyldobs,yieldUnits,pResids,hline,validation"
  plotList$p9$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    if (!validation){ 
      
      plotObs<-Obs
      plotpredict<-ppredict
      plotyldobs<-pyldobs
      plotyldpredict<-pyldpredict
      plotResids<-pResids
      
      p<-diagnoticPlots_4panel_A(plotpredict,plotObs,plotyldpredict,plotyldobs,sitedata,plotResids,plotclass=NA,
                                 plotTitles = c("'MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Load'",
                                                "'MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Yield'",
                                                "'Residuals vs Predicted \nLoad'",
                                                "'Residuals vs Predicted \nYield'"),
                                 loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                 pnch,markerCols,hline,filterClass = NA)
      
      return(p)
    }
  }#end p9 func
  #######################################################
  plotList$p10<-list()
  plotList$p10$title<-"ModSimulation_Box_and_Quantile_Resid"
  plotList$p10$header<-"# Model Simulation Performance Diagnostics
Diagnostics are based on the use of unconditioned predictions (i.e., predictions that are not adjusted for monitoring loads). These predictions (and the associated residuals and observed to predicted ratios shown in the following section) provide the best measure of the predictive skill of the estimated model in simulation mode. The simulated predictions are computed using mean coefficients from the NLLS model estimated with monitoring-adjusted (conditioned) predictions. \n
Four-plot panel for boxplots of residuals and observed/predicted ratios, normal quantile plot of standardized residuals, and plot of squared residuals vs. predicted loads"  
 
  plotList$p10$plotParams<-"pResids,pratio.obs.pred,
                                     ppredict,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,validation"
  plotList$p10$plotFunc<-function(plotParams.list){
    
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    if (!validation){  
      plotResids<-pResids
      plot.ratio.obs.pred<-pratio.obs.pred
      plot.standardResids<-NA
      plotpredict<-ppredict
      
      p<-diagnoticPlots_4panel_B(sitedata,plotResids,plot.ratio.obs.pred,plot.standardResids,plotpredict,
                                 plotTitles = c("'MODEL SIMULATION PERFORMANCE \nResiduals'",
                                                "'MODEL SIMULATION PERFORMANCE \nObserved / Predicted Ratio'",
                                                "'Normal Q-Q Plot'",
                                                "'Squared Residuals vs Predicted Load'"),
                                 loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                 pnch,markerCols,hline)
      assign("Resids2",Resids2,env = parent.frame())
      
      return(p)
    }
  }#end p2 func
  
  plotList$p11<-plotList$p4
  plotList$p11$title<-gsub("Estimation","Simulation",plotList$p11$title)
  
  plotList$p12<-plotList$p5
  plotList$p12$title<-gsub("Estimation","Simulation",plotList$p12$title)
  
  plotList$p13<-plotList$p6
  plotList$p13$title<-gsub("Estimation","Simulation",plotList$p13$title)
  
  plotList$p14<-plotList$p7
  plotList$p14$title<-gsub("Estimation","Simulation",plotList$p14$title)
  
  plotList$p15<-plotList$p8
  plotList$p15$title<-gsub("Estimation","Simulation",plotList$p15$title)
  
  ############################################
 return(plotList)
}