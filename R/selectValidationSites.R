#
# selectValidationSites.R
#
##############################################
# Two methods are available for validation site selection:
# 1. In cases where the 'pvalidate' control setting is > 0, a random selection of sites from the 'calsite' designated sites is executed.
#  The control setting 'pvalidate' specifies the decimal fraction of the monitoring sites as validation sites.

# 2. User-defined validation site selection uses the required variable name 'valsites', which must be defined by the user 
#     in the 'subdata' object, either by placement in the original 'data1.csv' file or as a statement in the 
#     'userModifyData.R' subroutine.
#  The variable 'calsites' should be defined as is typical for all possible calibration sites (0=not selected; 1=selected).
#  The variable 'valsites' should define the subset of 'calsites' to be used for validation (0=not selected; 1=selected).
#  Use the following control file settings:
#    if_validate == "yes"
#    pvalidate == 0 (user-defined validation site selection triggered by this setting)

 selectValidationSites <- function(iseed,pvalidate,subdata,minimum_reaches_separating_sites,data_names,batch_mode,ErrorOccured) {
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
   
  numrchs <- length(waterid)
  minnum <- minimum_reaches_separating_sites
  
  set.seed(iseed)
  nsamples <- round( (1-pvalidate) * max(staid))  # number of calibration sites
  rpick <- sample(rep(1:max(staid)),nsamples)
  tstaid <- staid
  staid <- numeric(numrchs)
  vstaid <- numeric(numrchs)
  vdepvar <- numeric(numrchs)
  nMon <- 0
  vic <- 0
  
  if(pvalidate > 0) { 
    message("   Random selection of validation sites applied.")
    
   # Default method using random selection of validation sites
   for (i in 1:length(depvar)) {
    if (tstaid[i] > 0) {
      iset <- 0
      for (j in 1:nsamples) {
        if (tstaid[i] == rpick[j]) {   
          if(depvar[i] > 0) {          # accepted site with non-zero load
            nMon <- nMon+1
            iset <- 1
            staid[i] <- nMon
          }
        } 
      }
      if (iset == 0) {   # site not picked, use for validation site
        staid[i] <- 0
        if(depvar[i] > 0) {
          vic <- vic+1
          vdepvar[i] <- depvar[i]    # store validation dependent variable value
          vstaid[i] <- vic
          depvar[i] <- 0
        }
      }
    }
   }
    
  } else {
    
    message("   User-defined validation site selection executed.")
    
   # User-define validation sites (pvalidate==0)
   for (i in 1:length(depvar)) {
    if(valsites[i]==1) {
      if(depvar[i] > 0) {
        staid[i] <- 0
        vic <- vic+1
        vdepvar[i] <- depvar[i]    # store validation dependent variable value
        vstaid[i] <- vic
        depvar[i] <- 0
      } else {
        staid[i] <- 0   # eliminate site from tagging as possible calibration site
      }
    } 
    if(calsites[i]==1 & valsites[i] != 1) {
      if(depvar[i] > 0) {
        nMon <- nMon+1
        staid[i] <- nMon
      } else {
        staid[i] <- 0
      }
    }
   }
    
  }  # end validation site selection method
  
  
  # obtain final 'staidseq' based on final site selection
  sid <- staid  
  staidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode,batch_mode,ErrorOccured)   # call function
  
  # obtain final 'vstaidseq' based on final site selection
  sid <- vstaid  
  vstaidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode,batch_mode,ErrorOccured)   # call function
  vdepvar <- ifelse(vdepvar > 0 & vstaidseq != sid,0,vdepvar) # eliminate sites with < min. number of separation reaches
  ncheck <- sum(ifelse(vdepvar == 0,0,1))
  
  # obtain final station count and loads, if any sites eliminated because < min. number separation reaches
  if(ncheck < vic) {
    # Assign validation sequence numbers
    vic <- 0
    for (i in 1:numrchs) {
      if (is.na(vstaid[i]) | vdepvar[i] == 0) {
        vstaid[i] <- 0
      } else {
        vic<-vic+1
        vstaid[i] <- vic
      }
    }
    # obtain final 'vstaidseq' (with updated vic sequence number) based on final validation site numbering
    sid <- vstaid 
    minnum <- minimum_reaches_separating_sites
    vstaidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode,batch_mode,ErrorOccured)   # call function
  }
  
  Vsites.list <- named.list(waterid,staid,vstaid,depvar,vdepvar,nMon,vic,staidseq,vstaidseq)

     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("selectValidationSites.R",batch_mode)
       }
     }else{#if no error
  return(Vsites.list)
     }#end if error
     
   }#test if previous error
 }#end function


