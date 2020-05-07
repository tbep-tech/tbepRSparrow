#
# predictSensitivity.R
#
#####################################################################################################
# Calculates predictions of load for sensitivity analysis

 predictSensitivity <- function(AEstimate,estimate.list,DataMatrix.list,SelParmValues,
                     reach_decay_specification,reservoir_decay_specification,subdata,batch_mode,ErrorOccured) {
  
  # INPUT objects:
  # estimate.list
  # DataMatrix.list
  # SelParmValues
  # reach_decay_specification
  # reservoir_decay_specification
  # subdata

#
# Output loads:
#   pload_total                Total load (fully decayed)
#   pload_(sources)            Source load (fully decayed)
#   mpload_total               Monitoring-adjusted total load (fully decayed)
#   mpload_(sources)           Monitoring-adjusted source load (fully decayed)
#   pload_nd_total             Total load delivered to streams (no stream decay)
#   pload_nd_(sources)         Source load delivered to streams (no stream decay)
#   pload_inc                  Total incremental load delivered to streams
#   pload_inc_(sources)        Source incremental load delivered to streams
#   deliv_frac                 Fraction of total load delivered to terminal reach
#   pload_inc_deliv            Total incremental load delivered to terminal reach
#   pload_inc_(sources)_deliv  Source incremental load delivered to terminal reach
#   share_total_(sources)      Source shares for total load (percent)
#   share_inc_(sources)        Source share for incremental load (percent)

# Output yields:
#   yield_total                Total yield (fully decayed)
#   yield_(sources)            Source yield (fully decayed)
#   myield_total               Monitoring-adjusted total yield (fully decayed)
#   myield_(sources)           Monitoring-adjusted source yield (fully decayed)
#   yield_inc                  Total incremental yield delivered to streams
#   yield_inc_(sources)        Source incremental yield delivered to streams
#   yield_inc_deliv            Total incremental yield delivered to terminal reach
#   yield_inc_(sources)_deliv  Source incremental yield delivered to terminal reach

#################################################
   if (ErrorOccured=="no"){
     tryIt<-try({ 

  # create global variable from list names (JacobResults)
  # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
  # 'Parmnames' list of variable names 
  for(i in 1:length(estimate.list$JacobResults)){
    tempobj=estimate.list$JacobResults[[i]]
    eval(parse(text=paste(names(estimate.list$JacobResults)[[i]],"= tempobj")))
  }
   # transfer required variables to global environment from SUBDATA
   datalstCheck <- as.character(getVarList()$varList)
   for (i in 1:length(datalstCheck)) {
     dname <- paste("subdata$",datalstCheck[i],sep="") 
     x1name <- paste(datalstCheck[i],sep="")
     if((x1name %in% names(subdata)) == TRUE) {
       assign(datalstCheck[i],eval(parse(text=dname)))
     }
   }

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
  
  # Setup variables for prediction 
  
  nreach <- length(data[,1])
  numsites <- sum(ifelse(data[,10] > 0,1,0))  # jdepvar site load index
  
  # transfer estimated parameters into complete parameter vector (inclusive of non-estimated constants)
  # values are perturbed in subsequent function calls in sensitivity analysis
  betalst <- AEstimate 

  # Load the parameter estimates to BETA1
  beta1<-t(matrix(betalst, ncol=nreach, nrow=length(oEstimate)))

  # setup for REACH decay
  jjdec <- length(jdecvar)
  if(sum(jdecvar) > 0) { 
    rchdcayf <- matrix(1,nrow=nreach,ncol=1)
    for (i in 1:jjdec){
      rchdcayf[,1] <- rchdcayf[,1] * eval(parse(text=reach_decay_specification))
    }   
  } else {  
    rchdcayf <- matrix(1,nrow=nreach,ncol=1)
  }

  # setup for RESERVOIR decay
  jjres <- length(jresvar)
  if(sum(jresvar) > 0) {
    resdcayf <- matrix(1,nrow=nreach,ncol=1)
    for (i in 1:jjres){
      resdcayf[,1] <- resdcayf[,1] * eval(parse(text=reservoir_decay_specification))
    }
  } else { 
    resdcayf <- matrix(1,nrow=nreach,ncol=1)
  } 

  # Setup for SOURCE DELIVERY # (nreach X nsources)
  jjdlv <- length(jdlvvar)
  jjsrc <- length(jsrcvar)

  ddliv1 <- matrix(0,nrow=nreach,ncol=jjdlv)
  if(sum(jdlvvar) > 0) {
    for (i in 1:jjdlv){
     ddliv1[,i] <- (beta1[,jbdlvvar[i]] * data[,jdlvvar[i]])
    }
    ddliv2 <- matrix(0,nrow=nreach,ncol=jjsrc)
    ddliv2 <- exp(ddliv1 %*% t(dlvdsgn))
  } else {
   ddliv2 <- matrix(1,nrow=nreach,ncol=jjsrc)   # change ncol from =1 to =jjsrc to avoid non-conformity error (2-19-2013)
  }

  # Setup for SOURCE
  ddliv3 <- (ddliv2 * data[,jsrcvar]) * beta1[,jbsrcvar]
  if(sum(jsrcvar) > 0) {
    dddliv <- matrix(0,nrow=nreach,ncol=1)
    for (i in 1:jjsrc){
      dddliv[,1] <- dddliv[,1] + ddliv3[,i]
    }
  } else {
    dddliv <- matrix(1,nrow=nreach,ncol=1)
  }

  ####################################################
  # incremental delivered load for decayed and nondecayed portions

  incdecay <- rchdcayf**0.5 * resdcayf   # incremental reach and reservoir decay
  totdecay <- rchdcayf * resdcayf        # total reach and reservoir decay

  incddsrc <- rchdcayf**0.5 * resdcayf * dddliv
  incddsrc_nd <- dddliv

  # Compute the reach transport factor
  carryf <- data[,jfrac] * rchdcayf * resdcayf
  carryf_nd <- data[,jfrac] 

  ####################################################
  # Store the incremental loads for total and sources

  pload_inc <- as.vector(dddliv)  # create incremental load variable

  srclist_inc <- character(length(jsrcvar))

  for (j in 1:length(jsrcvar)) {  
    ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
    assign(paste("pload_inc_",Parmnames[j],sep=""),as.vector(ddliv))   # create variable 'pload_inc_(source name)'
    srclist_inc[j] <- paste("pload_inc_",Parmnames[j],sep="")
  }

  ####################################################
  # Store the total decayed and nondecayed loads

      nnode <- max(data[,jtnode],data[,jfnode])
      ee <- matrix(0,nrow=nreach,ncol=1)
      pred <- matrix(0,nrow=nreach,ncol=1)

      i_obs <- 1

        data2 <- matrix(0,nrow=nreach,ncol=4)
        data2[,1] <- data[,jfnode]
        data2[,2] <- data[,jtnode]
        data2[,3] <- data[,jdepvar]
        data2[,4] <- data[,jiftran]

        
  # Total decayed load (no monitoring adjustment)
        
        incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
        carryf <- ifelse(is.na(carryf),0,carryf)
        ifadjust <- 0     # no monitoring load adjustment
        
  # accumulate loads
    return_data <- .Fortran('ptnoder',
     ifadjust=as.integer(ifadjust),
     nreach=as.integer(nreach),
     nnode=as.integer(nnode),
     data2=as.double(data2),
     incddsrc=as.double(incddsrc),
     carryf=as.double(carryf),
     ee=as.double(ee),PACKAGE="ptnoder") 
    pred <- return_data$ee
    
    pload_total <- pred   # nonadjusted total load 

  
  
     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("predictSensitivity.R",batch_mode)
       }
     }else{#if no error
       return(pload_total)
     }#end if error
     
   }#test if previous error
 }#end function
    
