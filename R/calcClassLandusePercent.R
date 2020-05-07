#'@title calcClassLandusePercent
#'@description function to calculate user selected class_landuse_percentages.  
#'Uses subroutines: accumulateIncrArea, errorOccurred. 
#'@param subdata input data (subdata)
#'@param class_landuse character vector of class_landuses from sparrow_control
#'@param class_landuse_percent numeric vector of percentages to apply to class_landuses from sparrow_control
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return data.frame with calculated percentages by land use



calcClassLandusePercent<-function(subdata,class_landuse, class_landuse_percent,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 
  
   #get data
  data<-subdata
  
  #get subset of data that matches user input class_landuse_percent
  for (i in class_landuse){
    #accumulate incremental landuse
    areaIncr<-accumulateIncrArea(data,i,"totalIncrArea",batch_mode,ErrorOccured)
    
    #get as percent of demtarea
    percentDemt<-merge(data[c("waterid","demtarea")],areaIncr, by ="waterid")
    percentDemt$percentLanduse<-percentDemt$totalIncrArea/percentDemt$demtarea*100
    
    #subset by user selected class_landuse_percent
    percentSub<-data.frame(waterid = percentDemt[which(percentDemt$percentLanduse>=class_landuse_percent[which(class_landuse==i)]),]$waterid)
    if (nrow(percentSub)!=0){
    percentSub$landuse<-i}
    
    #save
    if (i==class_landuse[1]){
      if (nrow(percentSub)!=0){
     percentClassLanduse<-percentSub
      }else{
        percentClassLanduse<-as.data.frame(matrix(c(1,"1"),ncol=2,nrow=0))
        names(percentClassLanduse)<-c("waterid","landuse")
     } 
    }else{
      if (nrow(percentSub)!=0){
      percentClassLanduse<-rbind(percentClassLanduse,percentSub)}
    }
  }#end for each class_landuse
    
  
    },TRUE)#end try

if (class(tryIt)=="try-error"){#if an error occured
  if(ErrorOccured=="no"){
    errorOccurred("calcClassLandusePercent.R",batch_mode)
  }
}else{#if no error
  return(percentClassLanduse)
}#end if error

  }#test if previous error
}#end function