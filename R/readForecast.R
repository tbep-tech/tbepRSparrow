readForecast<-function(file.output.list,forecast_filename,data_names,srcvar, use_sparrowNames, batch_mode){
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  filefData <- paste0(path_data,forecast_filename)   
  
  fData <- read.csv(filefData,header=TRUE,stringsAsFactors=FALSE,
  dec = csv_decimalSeparator,sep=csv_columnSeparator,
  fileEncoding="UTF-8-BOM")
  
  #test for names in data_names
  if (!use_sparrowNames){
  for (n in names(fData)){
    if (!n %in% data_names$data1UserNames){
      message(paste0(n,"  NOT FOUND in dataDictionary.REMOVED FROM FORECASTING SCENARIO"))
      fData<-fData[,names(fData)!=n]
    }else{
    #check if in srcvar
    if (!as.character(na.omit(data_names[data_names$data1UserNames==n,]$sparrowNames)) %in% srcvar & 
        as.character(na.omit(data_names[data_names$data1UserNames==n,]$sparrowNames))!="waterid"){
      message(paste0(n,"  NOT FOUND in SOURCE or DELIV params REMOVED FROM FORECASTING SCENARIO"))
     fData<-fData[!names(fData)!=n] 
    }else{
      names(fData)[names(fData)==n]<-as.character(na.omit(data_names[data_names$data1UserNames==n,]$sparrowNames)) 
    }
    }
  }#for each n
  }else{#use sparrowNames
    for (n in names(fData)){
      if (!n %in% data_names$sparrowNames){
        message(paste0(n,"  NOT FOUND in dataDictionary.REMOVED FROM FORECASTING SCENARIO"))
        fData<-fData[,names(fData)!=n]
      }else{
        #check if in srcvar
        if (!n %in% srcvar & n!="waterid"){
          message(paste0(n,"  NOT FOUND in SOURCE or DELIV params REMOVED FROM FORECASTING SCENARIO"))
          fData<-fData[!names(fData)!=n] 
        }
      }
    }
  }
  
  #test for waterid
  if (!any(names(fData)=="waterid")){
    message("ERROR : waterid NOT FOUND IN FORECAST SCENARIO DATA. SCENARIO NOT RUN")
    errorOccurred("readForecast.R",batch_mode)
  }
  
  #test vars exist
  if (length(fData)==1){
    message("ERROR : NO PARAMS FOUND IN FORECAST SCENARIO DATA. SCENARIO NOT RUN")
    errorOccurred("readForecast.R",batch_mode) 
  }
  
  
  
  return(fData)
  
}