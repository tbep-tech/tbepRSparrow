diagnoticPlots_4panel_B<-function(sitedata,plotResids,plot.ratio.obs.pred,plot.standardResids,plotpredict,
                                  plotTitles,loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                  pnch,markerCols,hline){
  
  # Residual plots
  p1 <- plotlyLayout(NA,plotResids, log = "", nTicks = 4, digits = 0,
                     xTitle = "",  xZeroLine = FALSE,
                     yTitle = "LOG RESIDUAL",  yZeroLine = FALSE,
                     plotTitle = eval(parse(text = plotTitles[1])),
                     legend = FALSE,showPlotGrid = showPlotGrid)
  p1 <- p1 %>% add_trace(y = plotResids,type = 'box', name = "Resids", color = I("black"), 
                         fillcolor = "white")
  
  
  # Obs-Pred ratio boxplot
  
  p2 <- plotlyLayout(NA,plot.ratio.obs.pred, log = "", nTicks = 4, digits = 0,
                     xTitle = "", xZeroLine = FALSE,
                     yTitle = "RATIO OBSERVED TO PREDICTED", yZeroLine = FALSE,
                     plotTitle = eval(parse(text = plotTitles[2])),
                     legend = FALSE,showPlotGrid = showPlotGrid)
  p2 <- p2 %>% add_trace(y = plot.ratio.obs.pred,type = 'box', name = "Ratio", color = I("black"), fillcolor = "white")
  
  
  
  # Normality probability plot
  if(!is.na(plot.standardResids)){
  markerText<-"~paste('</br> Standard Residual: ',plot.standardResids,
                      '</br> Theoretical Value: ',y.theo"
  yTitle<-"Standardized Residuals"
  data<-data.frame(plot.standardResids = plot.standardResids)
  }else{
    markerText<-"~paste('</br> Log Residual: ',plotResids,
                      '</br> Theoretical Value: ',y.theo"
    yTitle<-"Log Residuals"
    data<-data.frame(plot.standardResids = plotResids) 
  }
  
  
  markerText<-addMarkerText(markerText,add_plotlyVars,data, sitedata)$markerText
  data<-addMarkerText(markerText,add_plotlyVars, data,sitedata)$mapData
  
  data<-data[order(data$plot.standardResids),]
  
  #get f values for data
  i<-1:nrow(data)
  fi<-(i-0.5)/nrow(data)
  x.norm<-qnorm(fi)
  #find 1st and 3rd quantiles
  y <- quantile(data$plot.standardResids, c(0.25, 0.75), type = 5)
  x <- qnorm( c(0.25, 0.75))
  
  #get qqline
  slope <- diff(y) / diff(x)
  int   <- y[1] - slope * x[1]
  y.theo <- sapply(x.norm, function(k) as.numeric(slope)*k+as.numeric(int))
  
  data$x.norm<-x.norm
  data$y.theo<-y.theo
  
  #plot data points on qq plot
  p3 <- plotlyLayout(data$x.norm,data$plot.standardResids, log = "", nTicks = 7, digits = 0,
                     xTitle = "Theoretical Quantiles", xZeroLine = FALSE,
                     yTitle = yTitle, yZeroLine = FALSE,
                     plotTitle = "Normal Q-Q Plot",
                     legend = FALSE,showPlotGrid = showPlotGrid)
  
  p3 <- p3 %>% add_trace(data = data, x = ~x.norm, y = ~plot.standardResids, 
                         type = "scatter", 
                         mode = "markers",
                         marker = eval(parse(text = markerList)),
                         hoverinfo = 'text',
                         text = eval(parse(text = markerText)))
  p3 <- p3 %>% add_trace(data = data, x = ~x.norm, y = ~y.theo, 
                         type = "scatter", 
                         mode = "lines",
                         color = I("red"),
                         hoverinfo = 'text',
                         text = "Theoretical Normal Distribution")
  
  
  # Squared residuals vs predicted
  Resids2 <- plotResids**2
  assign("Resids2",Resids2,env = parent.frame())
  
  lwresids <- lowess(plotpredict,Resids2, f = 0.5, iter = 3)
  
  lwy <- lwresids$y[1:(length(lwresids$y)-1)]
  lwx <- lwresids$x[1:(length(lwresids$x)-1)]
  
  # lines(lwx,lwy,col=2)
  markerText<-"~paste('</br> Squared Log Residual: ',Resids2,
                   '</br> Predicted Load: ',plotpredict"
  data<-data.frame(Resids2 = Resids2,plotpredict = plotpredict)
  
  markerText<-addMarkerText(markerText,add_plotlyVars,data, sitedata)$markerText
  data<-addMarkerText(markerText,add_plotlyVars, data,sitedata)$mapData

  p4 <- plotlyLayout(plotpredict,Resids2, log = "xy", nTicks = 5, digits = 0,
                     xTitle = paste0("PREDICTED LOAD (",loadUnits,")"), xZeroLine = FALSE,
                     yTitle = "SQUARED LOG RESIDUALS", yZeroLine = FALSE,
                     plotTitle = "Squared Residuals vs Predicted Load",
                     legend = FALSE,showPlotGrid = showPlotGrid)
  p4 <- p4 %>% add_trace(data = data, x = ~plotpredict, y = ~Resids2, 
                         type = "scatter", 
                         mode = "markers",
                         marker = eval(parse(text = markerList)),
                         hoverinfo = 'text',
                         text = eval(parse(text = markerText)))
  p4 <- p4 %>% add_trace(x = lwx, y = lwy, 
                         type = "scatter", 
                         mode = "lines",
                         color = I("red"),
                         hoverinfo = 'text',
                         text = "Lowess smooth line")
  p<-subplot(p1,p2,p3,p4,nrows = 2, widths = c(0.5,0.5), heights = c(0.5, 0.5),
             titleX = TRUE, titleY=TRUE, margin = 0.1)
  return(p)
}