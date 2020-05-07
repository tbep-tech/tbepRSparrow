#
# selectCalibrationSites.R
#
##############################################

selectCalibrationSites <- function(subdata,data_names,
                                   minimum_headwater_site_area,
                                   minimum_reaches_separating_sites,
                                   minimum_site_incremental_area,
                                   batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 
 
  ################################################
  # create global variables for calculations

  datalstreq <- data_names$sparrowNames
  for (i in 1:length(datalstreq)) {
    dname <- paste("subdata$",datalstreq[i],sep="")
    x1name <- paste(datalstreq[i],sep="")
    if((x1name %in% names(subdata)) == TRUE) {
      assign(x1name,eval(parse(text=dname)))
    }
  }
  
  ##################################################
  # Select site loads for calibration based on 'calsites' index value
  depvar <- ifelse(calsites == 1,depvar,0)

  #################################################
  # Count reaches and sites
  numrchs <- length(waterid)
  depvar <- ifelse(is.na(depvar),0,depvar)
  numsites1 <- sum(ifelse(depvar == 0,0,1))
  
  ###################
  # initialize staid sequence based on depvar (avoids case of missing staid by user; added 1-8-2017)
  nn <- 0
  for (i in 1:numrchs) {
    if (depvar[i] == 0) {
      staid[i] <- 0
    } else {
      nn<-nn+1
      staid[i] <- nn
    }
  }

  # check staid
#  cc <- staid8[staid8 != 0]   # sites
# Number of headwater drainage areas with monitoring sites
#summary(demtarea[headflag==1])
#xy <- subset(subdata,subdata$headflag==1 & depvar > 0) 
#length(xy$demiarea)
#summary(xy$demiarea)

  # 1. Eliminate headwater monitoring sites with small areas
  minharea <- minimum_headwater_site_area
  depvar <- ifelse(demiarea < minharea & headflag==1 & depvar > 0,0,depvar)
  numsites2 <- sum(ifelse(depvar == 0,0,1))

  # 2. Select sites with a minimum number of reaches separating sites
  minnum <- minimum_reaches_separating_sites
  sid <- ifelse(depvar > 0,staid,0)
  staidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode,batch_mode,ErrorOccured)   # call function
  depvar <- ifelse(depvar > 0 & staidseq != sid,0,depvar)  # eliminate sites
  numsites3 <- sum(ifelse(depvar == 0,0,1))

  # 3. Select sites with minimum incremental drainage area (km2)
  seliarea <- minimum_site_incremental_area
  sid <- ifelse(depvar > 0,staid,0)   # recode for reduced set of sites (8-26-2016)
  xx <- data.frame(waterid,sid,staidseq,demiarea,depvar,hydseq,demtarea,tnode,fnode)
  count<-ddply(xx,.(staidseq), summarize, nirchs=length(staidseq))      # get count for unique staids
  siteiarea<-ddply(xx,.(staidseq),summarize,tiareakm2=sum(demiarea))    # sum incr areas for unique staids
  xx <- merge(xx,siteiarea,by="staidseq",all.y=FALSE,all.x=TRUE)
  xx <- merge(xx,count,by="staidseq",all.y=FALSE,all.x=TRUE)
  xx <- xx[with(xx,order(xx$hydseq)), ]     # resort dataframe by the original HYDSEQ order
  depvar <- ifelse(xx$depvar > 0 & xx$tiareakm2 < seliarea & xx$sid != 0,0,xx$depvar)  # eliminate sites
  numsites4 <- sum(ifelse(depvar == 0,0,1))
  
  waterid <- xx$waterid
  staid <- xx$sid
  staidseq <- xx$staidseq
  depvar <- xx$depvar
  tnode <- xx$tnode
  fnode <- xx$fnode

  #################################################
  # Assign STAID sequence numbers
  nMon <- 0
  for (i in 1:numrchs) {
    if (is.na(staid[i]) | depvar[i] == 0) {
      staid[i] <- 0
    } else {
      nMon<-nMon+1
      staid[i] <- nMon
    }
  }
  
  # obtain final 'staidseq' based on final site selection
  sid <- staid   # ifelse(depvar > 0,staid,0)
  staidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode,batch_mode,ErrorOccured)   # call function
  
  Csites.list <- named.list(waterid,depvar,staid,numsites1,numsites2,numsites3,numsites4,nMon,staidseq)

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("selectCalibrationSites.R",batch_mode)
      }
    }else{#if no error
  return(Csites.list)
    }#end if error
    
  }#test if previous error
}#end function
