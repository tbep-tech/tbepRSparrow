#
# validateMetrics.R
#
#################################################

validateMetrics <- function(classvar,classrchvar,estimate.list,dlvdsgn,Vsites.list,yieldFactor,
                            SelParmValues,subdata,vsitedata,DataMatrix.list,batch_mode,ErrorOccured) {
  
  # INPUT objects
  # classrchvar
  # classvar
  # Csites.list
  # estimate.list
  # SelParmValues
  # subdata
  # vsitedata
  # DataMatrix.list
  # dlvdsgn
  
  # OUTPUT objects (validate.metrics.list):
  # ANOVA.list
  # Mdiagnostics.list
  if (ErrorOccured=="no"){
    tryIt<-try({

  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(vsitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(vsitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("vsitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  } 
  # contiguous class variables by reach
  classrch <- as.numeric(eval(parse(text=paste("subdata$",classrchvar,sep=""))))  # used to compute RMSE by class
  
  data <- DataMatrix.list$data
  
  # create global variable from list names
  for(i in 1:length(SelParmValues)){
    tempobj=SelParmValues[[i]]
    eval(parse(text=paste(names(SelParmValues)[[i]],"= tempobj")))
  }
  # transfer required variables to global environment from 'DataMatrix.list$data.index.list'
  for(i in 1:length(DataMatrix.list$data.index.list)){
    tempobj=DataMatrix.list$data.index.list[[i]]
    eval(parse(text=paste(names(DataMatrix.list$data.index.list)[[i]],"= tempobj")))
  }
  # transfer required variables to global environment from SUBDATA
  datalstCheck <- c(as.character(getVarList()$varList),"vdepvar","vstaid")
  for (i in 1:length(datalstCheck)) {
    dname <- paste("subdata$",datalstCheck[i],sep="") 
    x1name <- paste(datalstCheck[i],sep="")
    if((x1name %in% names(subdata)) == TRUE) {
      assign(datalstCheck[i],eval(parse(text=dname)))
    }
  }

##########################################################
# SUMMARY PERFORMANCE METRICS FOR NO MONITORING ADJUSTMENT

  Estimate <- estimate.list$JacobResults$oEstimate[SelParmValues$betaconstant==0]
  pResids <- validateFevalNoadj(Estimate,vdepvar,batch_mode,ErrorOccured)

   # Obtain predicted, observed, residual values
   nreach <- length(DataMatrix.list$data[,1])
   numsites <- Vsites.list$vic
   npar <- length(Estimate)
   mobs <- numsites
   Obs <- numeric(numsites)
   xstaid <- numeric(numsites)
   tarea <- numeric(numsites)
   xlat <- numeric(numsites)
   xlon <- numeric(numsites)
   ppredict <- numeric(numsites)
   pyldobs <- numeric(numsites)
   pyldpredict <- numeric(numsites)
   pratio.obs.pred <- numeric(numsites)
   pxssemrb <- numeric(max(class[,1]))
   
   ins <- 0
   for (k in 1:nreach) {
    if (vdepvar[k] > 0) {
       ins <- ins+1
       obs <- vdepvar[k]                        # data[k,jdepvar]
       Obs[ins] <- vdepvar[k]                   # data[k,jdepvar]
       ppredict[ins] <- (exp(log(obs) - pResids[ins]))  
       pyldobs[ins] <- Obs[ins] / data[k,jtotarea] * yieldFactor
       pyldpredict[ins] <- ppredict[ins] / data[k,jtotarea] * yieldFactor
       pratio.obs.pred[ins] <- Obs[ins] / (ppredict[ins])
       
       xlat[ins] <- lat[k]
       xlon[ins] <- lon[k]
       xstaid[ins] <- vstaid[k]
       tarea[ins] <- data[k,jtotarea]
       
   # compute SSE by MRB
       if(class[ins] > 0) {
        pxssemrb[class[ins,1]] <- pxssemrb[class[ins,1]] + pResids[ins]^2
       }
    }                                 
   }  #  reach counter 
   pxssemrb[is.na(pxssemrb)] <- 0
   
   
   DF <- mobs-npar
   pSSE <- sum(pResids**2)
   pMSE <- pSSE / DF
   pRMSE <- sqrt(pMSE)

   pRSQ <- 1 - pSSE / (sum(log(Obs)^2) - sum(log(Obs))^2/mobs)
   pRSQ_ADJ <- 1 - ((mobs - 1)/DF)*(1 - pRSQ) 
   pRSQ_YLD <- 1 - pSSE / (sum(log(pyldobs)^2) - sum(log(pyldobs))^2/mobs)

   NSn <- 0
   NSd <- 0
   PBiasn <- 0
   PBiasd <- 0
   Obsmean <- mean(log(Obs))
   for (i in 1:mobs) {
     NSn <- NSn + (log(Obs[i]) - log(ppredict[i]))^2
     NSd <- NSd + (log(Obs[i]) - Obsmean)^2
     PBiasn <- PBiasn + (Obs[i] - ppredict[i])
     PBiasd <- PBiasd + Obs[i]
   }
   pNSeff <- 1 - (NSn / NSd)    # overall Nash-Sutcliffe model efficiency
   pPBias <- PBiasn / PBiasd * 100   # Percent bias
   
   mrbgrp <- table(class[,1])   # get labels
   xx <- as.data.frame(mrbgrp)  # convert table to dataframe...
   mrbgrp <- as.numeric(levels(xx$Var1)[xx$Var1]) 
   
   
  # Compute SSE and RMSE for MRB regions
  pSSEMRB <- numeric(length(mrbgrp))
  pRMSEMRB <- numeric(length(mrbgrp))
  ii <- 0
  for (k in 1:length(pxssemrb)) {
    if(pxssemrb[k] != 0) {
      ii <- ii+1
      pSSEMRB[ii] <- pxssemrb[k]
  # pRMSEMRB[ii] <- sqrt(pxssemrb[k] / (xx$Freq[ii]-mrb_varct[k]))
    }
  }

  vANOVA.list <- named.list(mobs,npar,DF,pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pNSeff,pPBias)
  vMdiagnostics.list <- named.list(Obs,xstaid,tarea,xlat,xlon,
                                  ppredict,pyldobs,pyldpredict,pratio.obs.pred,pxssemrb,
                                  mrbgrp,pRMSEMRB,pSSEMRB,pResids)

  validate.metrics.list <- named.list(vANOVA.list,vMdiagnostics.list)
  

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("validateMetrics.R",batch_mode)
      }
    }else{#if no error
  return(validate.metrics.list)
    }#end if error
    
  }#test if previous error
}#end function
