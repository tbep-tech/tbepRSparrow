modelCompare<-function(path_results,file_sum,compare_models,modelComparison_name,
                       if_auto_scaling,if_diagnostics,csv_decimalSeparator, csv_columnSeparator,
                       batch_mode,ErrorOccured){
  
  if (ErrorOccured=="no"){
    tryIt<-try({ 

    # define title output function
  outcharfun<-function(char) {
    outchar <- data.frame(char)
    row.names(outchar ) <- c(" ")
    colnames(outchar ) <- c(" ")
    return(outchar)
  }
  # define "space" for printing
  ch <- character(1)
  space <- data.frame(ch)
  row.names(space) <- ch
  colnames(space) <- c(" ")
  
  #compare results of previous models
  if (length(na.omit(compare_models))!=0){
    message("Running Model Comparison...")
    #get current model data
    path_summary<-paste(path_results,"/estimate/summaryCSV/",sep="")
    modelPerformance<-read.csv(file=paste(path_summary,"ModelPerformanceMonitoringAdj.csv",sep=""),
                               dec = csv_decimalSeparator,sep=csv_columnSeparator)
    ch <- " "
    row.names(modelPerformance) <- ch
    eigenSpread<-read.csv(file=paste(path_summary,"EigenValueSpread.csv",sep=""),
                          dec = csv_decimalSeparator,sep=csv_columnSeparator)
    row.names(eigenSpread) <- ch
    ifScale<-if_auto_scaling
    paramEst<-read.csv(file=paste(path_summary,"ParameterEstimates.csv",sep=""),
                       dec = csv_decimalSeparator,sep=csv_columnSeparator)
    
    if (if_diagnostics=="yes"){
      moran<-read.csv(file=paste(path_summary,"EuclideanMoransI.csv",sep=""),
                      dec = csv_decimalSeparator,sep=csv_columnSeparator)
    }
    
    #create directory for comparison
    options(warn = -1)
    dir.create(paste(dirname(path_results),"/",modelComparison_name,"/",sep=""))
    options(warn=0)
    sinkFile<-paste(dirname(path_results),"/",modelComparison_name,"/",modelComparison_name,"_summary.txt",sep="")
    sink(file=sinkFile,split="FALSE",append=FALSE)
    
    #for sink to txt file
    print(outcharfun("SPARROW NLLS MODEL COMPARISION SUMMARY"))
    print(outcharfun(paste("MODEL COMPARISION NAME: ",modelComparison_name,sep="")))
    print(space)
    print(space)
    print(outcharfun("SPARROW NLLS MODEL SUMMARY"))
    print(outcharfun(paste("MODEL NAME: ",file_sum,sep="")))
    print(outcharfun("MODEL PERFORMANCE (Monitoring adjustment)"))
    print(modelPerformance)
    print(space)
    if(ifScale == "yes") {
      print(outcharfun("Parameter scaling applied for model estimation"))
    } else {
      print(outcharfun("Parameter scaling not applied for model estimation"))
    }
    print(space)
    print(outcharfun("PARAMETER ESTIMATES"))
    print(paramEst)
    print(space)
    print(outcharfun("EigenValue Spread"))
    print(eigenSpread)
    print(space)
    if (if_diagnostics=="yes"){
      print(outcharfun("Euclidean Moran's I"))
      print(moran)
      print(space)
    }
    
    #add rownames
    ch<-1
    row.names(modelPerformance) <- ch
    row.names(eigenSpread) <- ch
    
    #add model name
    modelPerformance$modelName<-file_sum
    eigenSpread$modelName<-file_sum
    paramEst$modelName<-file_sum
    if (if_diagnostics=="yes"){
      moran$modelName<-file_sum
    }
    
    for (m in compare_models){
      path_summary<-paste(dirname(path_results),"/",m,"/estimate/summaryCSV/",sep="")
      if (dir.exists(path_summary)){
       #get model data
        cmodelPerformance<-read.csv(file=paste(path_summary,"ModelPerformanceMonitoringAdj.csv",sep=""),
                                    dec = csv_decimalSeparator,sep=csv_columnSeparator)
        ch <- " "
        row.names(cmodelPerformance) <- ch
        ceigenSpread<-read.csv(file=paste(path_summary,"EigenValueSpread.csv",sep=""),
                               dec = csv_decimalSeparator,sep=csv_columnSeparator)
        row.names(ceigenSpread) <- ch
        cifdiag<-read.csv(file=paste(dirname(path_results),"/",m,"/",m,"_userSettings.csv",sep=""),
                          dec = csv_decimalSeparator,sep=csv_columnSeparator)
        cifdiag<-as.character(cifdiag[which(cifdiag$setting=="if_diagnostics"),]$value)
        cifdiag<-gsub("\"","",cifdiag)
        cparamEst<-read.csv(file=paste(path_summary,"ParameterEstimates.csv",sep=""),
                            dec = csv_decimalSeparator,sep=csv_columnSeparator)
       
        cifScale<-read.csv(file=paste(dirname(path_results),"/",m,"/",m,"_userSettings.csv",sep=""),
                           dec = csv_decimalSeparator,sep=csv_columnSeparator)
        cifScale<-as.character(cifScale[which(cifScale$setting=="if_auto_scaling"),]$value)
        cifScale<-gsub("\"","",cifScale)
        if (cifdiag=="yes"){
          cmoran<-read.csv(file=paste(path_summary,"EuclideanMoransI.csv",sep=""),
                           dec = csv_decimalSeparator,sep=csv_columnSeparator)
        }
        
        #sink to txt file
        print(outcharfun("SPARROW NLLS MODEL SUMMARY"))
        print(outcharfun(paste("MODEL NAME: ",m,sep="")))
        print(outcharfun("MODEL PERFORMANCE (Monitoring adjustment)"))
        print(cmodelPerformance)
        print(space)
        if(cifScale == "yes") {
          print(outcharfun("Parameter scaling applied for model estimation"))
        } else {
          print(outcharfun("Parameter scaling not applied for model estimation"))
        }
        print(space)
        print(outcharfun("PARAMETER ESTIMATES"))
        print(cparamEst)
        print(space)        
        print(outcharfun("EigenValue Spread"))
        print(ceigenSpread)
        print(space)
        if (cifdiag=="yes"){
          print(outcharfun("Euclidean Moran's I"))
          print(cmoran)
          print(space)
        }
        #add rownames
        ch<-1
        row.names(cmodelPerformance) <- ch
        row.names(ceigenSpread) <- ch
        
        #add model name
        cmodelPerformance$modelName<-m
        ceigenSpread$modelName<-m
        cparamEst$modelName<-m
        if (cifdiag=="yes"){
          cmoran$modelName<-m
        }
        
        #compile for csv files
        modelPerformance<-rbind.fill(modelPerformance,cmodelPerformance)
        eigenSpread<-rbind.fill(eigenSpread,ceigenSpread)
        paramEst<-rbind.fill(paramEst,cparamEst)
        if (if_diagnostics=="yes"){
          if (cifdiag=="yes"){
            moran<-rbind.fill(moran,cmoran)
          }
        }
        
        
      }else{
        message(paste(path_summary," DOES NOT EXIST.  MODEL COMPARISON NOT RUN.",sep=""))
      }

    }#for each model to compare
    
    sink()
    #output csv files
    dirOUT<-dirname(sinkFile)
    fwrite(file=paste(dirOUT,"/",modelComparison_name,"_ModelPerformanceMonitoringAdj.csv",sep=""),modelPerformance,row.names=FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    fwrite(file=paste(dirOUT,"/",modelComparison_name,"_EigenValueSpread.csv",sep=""),eigenSpread,row.names=FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    fwrite(file=paste(dirOUT,"/",modelComparison_name,"_ParameterEstimates.csv",sep=""),paramEst,row.names=FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    if (if_diagnostics=="yes"){
      fwrite(file=paste(dirOUT,"/",modelComparison_name,"_EuclideanMoransI.csv",sep=""),moran,row.names=FALSE,
             dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
      }
  }#if models to compare
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured

      sink()
      if(ErrorOccured=="no"){
        errorOccurred("modelCompare.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function