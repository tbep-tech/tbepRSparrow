#'@title calcIncremLandUse
#'@description function calculate the land-use percentages for the incremental drainage areas
#'of the monitoring sites for use in decile diagnostics
#'Uses subroutines: sumIncremAttributes, errorOccurred.
#'@param subdata input data (subdata) 
#'@param class_landuse character vector of class_landuses from sparrow_control
#'@param idseq staidseq or vstaidseq
#'@param minimum_reaches_separating_sites number indicating the minimum number of reaches separating sites
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return data.frame incremental land-use percentages calculated by class_landuse
#
# calcIncremLandUse.R
#
##############################################

calcIncremLandUse <- function(subdata,class_landuse,idseq,minimum_reaches_separating_sites,batch_mode,ErrorOccured) { 
  if (ErrorOccured=="no"){
    tryIt<-try({  
  # INPUT objects:
  # idseq:  staidseq or vstaidseq

  ###########################################################
  # calculate the land-use percentages for the incremental drainage areas 
  #   of the monitoring sites for use in decile diagnostics
  ###########################################################
  #
  # setup_landuse_classes.R
  #
  # setup classvar2 classes in 'df' object for plotting diagnostics for non-contiguous variables:
  
  # Note that the setup code required for diagnostics for non-contiguous classification 
  # variables that require summation of attributes for the station incremental areas, such as land use.
  # Contiguous variables, such as HUC-4, do not require this step.
  
  # execute folowing:  classvar2 <- c("forest_pct","agric_pct","urban_pct","shrubgrass_pct")
  if(!is.na( class_landuse[1])){
    classvar2 <- character(length(class_landuse))
    for (i in 1:length(class_landuse)) {
      classvar2[i] <- paste(class_landuse[i],"_pct",sep="")
    }
  }
  
  # incremental site area 
  waterid <- subdata$waterid
  tnode <- subdata$tnode
  fnode <- subdata$fnode
  demiarea <- subdata$demiarea
  df <- data.frame(waterid,demiarea,idseq)
  siteiarea <- sumIncremAttributes(idseq,demiarea,"siteincarea",batch_mode,ErrorOccured)  # sum incremental area by unique siteIDs
  df <- siteiarea
#  df <- merge(df,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE) 
  
  # Code executes 'sumIncremAttributes' function for each land-use type:
  # Forest percentage example
  #siteiarea <- sumIncremAttributes(idseq,subdata$forest,"forest_pct")
  #df <- merge(df,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE)
  #df$forest_pct <- df$forest_pct / df$siteincarea * 100
  if(!is.na( class_landuse[1])){
  for (i in 1:length(class_landuse)){
    nclass <- paste("subdata$",class_landuse[i],sep="")
    nclasspct <- paste("df$",classvar2[i],sep="")
    xname <- paste("siteiarea <- sumIncremAttributes(idseq,",nclass,",",shQuote(classvar2[i]),",batch_mode,ErrorOccured)",sep="")
    eval((parse(text=xname)))
    
    df <- merge(df,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE)
    
    xname <- paste("df$",classvar2[i]," <- df$",classvar2[i]," / df$siteincarea * 100",sep="")
    eval((parse(text=xname)))
  }}
  
  # substitute 0.0 for NAs for user-selected parameters (assumes variables already present in 'df')
  setNAdf <- function(names){
    for (i in 1:length(names)) {
      dname <- paste("df$",names[i],"<-ifelse(is.na(df$",names[i],"),0.0,df$",names[i],")",sep="") 
      eval(parse(text=dname)) 
    }
  }
  if(!is.na( class_landuse[1])){
  setNAdf(classvar2)
  }
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("calcIncremLandUse.R",batch_mode)
      }
    }else{#if no error
      return(df)   
    }#end if error
    
  }#test if previous error
}#end function



