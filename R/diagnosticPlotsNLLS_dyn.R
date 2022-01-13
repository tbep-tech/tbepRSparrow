diagnosticPlotsNLLS_dyn<-function(diagnostic_params){
  unPackList(lists = list(diagnostic_params = diagnostic_params),
             parentObj = list(NA))
  plotList<-create_diagnosticPlotList()
  
  for (n in names(plotList)){
    htmlFile<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,
                            eval(parse(text = paste0("plotList$",n,"$title"))),".html")

    rmdTitle<-eval(parse(text = paste0("plotList$",n,"$title")))
    path_diagnosticPlotsNLLS_dynOutChild<-file_path_as_absolute(paste0(path_master,"diagnosticPlotsNLLS_dynChild.Rmd"))
    
    rmarkdown::render(paste0(path_master,"diagnosticPlotsNLLS_dynOut.Rmd"),
                      params = list(
                        rmdTitle = rmdTitle,
                        diagnostic_params = diagnostic_params,
                        plotIndex = n,
                        path_diagnosticPlotsNLLS_dynOutChild = path_diagnosticPlotsNLLS_dynOutChild
                      ),
                      output_file = htmlFile, quiet = TRUE
    )
    
    
  }
  

  htmlFile<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,
                   "Obs_v_Pred_TimeSeriesPlots.html")
  rmdTitle<-"Observed vs. Predicted Time Series Plots"
  path_diagnosticPlotsNLLS_timeSeriesChild<-file_path_as_absolute(paste0(path_master,"diagnosticPlotsNLLS_timeSeriesChild.Rmd"))
  
  rmarkdown::render(paste0(path_master,"diagnosticPlotsNLLS_timeSeries.Rmd"),
                    params = list(
                      rmdTitle = rmdTitle,
                      diagnostic_params = diagnostic_params,
                      path_diagnosticPlotsNLLS_timeSeriesChild = path_diagnosticPlotsNLLS_timeSeriesChild
                    ),
                    output_file = htmlFile, quiet = TRUE
  )
  
  
}