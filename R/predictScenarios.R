#
# predictScenarios.R
#
#####################################################################################################
# Calculates load predictions resulting from load-reduction scenarios for selected source variables

 predictScenarios <- function(#Rshiny
                              input,allMetrics, output_map_type,Rshiny,
                              #regular
                              predict.list,if_predict_scenarios,scenario_sources,scenario_all_factors,
                              data_names,estimate.list,ConcFactor, if_predict,
                              #bootcorrection,
                              DataMatrix.list,SelParmValues,
                              reach_decay_specification,reservoir_decay_specification,subdata,
                              #predictStreamMapScenarios
                              path_results,file_sum,path_gis,
                              #scenarios out
                              add_vars,csv_decimalSeparator,csv_columnSeparator,
                              mapping.input.list,
                              batch_mode,ErrorOccured) {
  #Rshiny input
  # #input$variable
  # #input$scLoadCheck
   #input$batch
  # #input$scYieldCheck
   #input$domain
################   #input$selectReaches
   #input$scenario_sources
   #input$factors
   ##new##
   #input$allSrc
   #input$selectFunc
   
   
  # INPUT objects:
  # predict.list
  # source.scenarios
  # scenario.factors
   
  # estimate.list
  # DataMatrix.list
  # SelParmValues
  # reach_decay_specification
  # reservoir_decay_specification
  # subdata
  # bootcorrection
  # ConcFactor

# 
# Output matrices in returned object 'predictScenarios.list':
#  predmatrix - absolute loads reflecting change from load-reduction scenarios
#  yldmatrix - absolute concentration and yields reflecting change from load-reduction scenarios
#  predmatrix_chg - change from baseline loads, expressed as ratio of new to baseline load
#  yldmatrix_chg - change from baseline concentration and yields, expressed as a ratio of new to baseline conditions
   
# Output load variables:
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

# Output yield variables:
#   Concentration              Concentration based on decayed total load and discharge
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
       if (Rshiny==TRUE |
           (Rshiny==FALSE & if_predict_scenarios != "none")){
         #from controlfiletaskmodel
         if (Rshiny==TRUE){
           scenario_name<-as.character(input$scenarioName)
           scenario_sources<-as.character(input$scenario_sources)
         }
           options(warn=-1)
           dir.create(paste(path_results,"/scenarios/",scenario_name,sep=""))
           options(warn=0)
          
           
           # Calculate and output with bias-corrected predictions
           if(is.null(estimate.list$JacobResults$mean_exp_weighted_error) == TRUE) {
             bootcorrection <- 1.0
           } else {
             bootcorrection <- estimate.list$JacobResults$mean_exp_weighted_error
           }
           if (file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep="")) | if_predict == "yes"){
             if (!exists("predict.list")){
               load(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))
             } 
             
             # perform checks on scenario variable names designated by user
             #  scenario only executed if all source variables match
             vcheck<-0
             for (i in 1:length(estimate.list$JacobResults$Parmnames)) {
               for (j in 1:length(scenario_sources)) {
                 if(scenario_sources[j] == estimate.list$JacobResults$Parmnames[i]) {vcheck<-vcheck+1}
               }
             }
             if(vcheck == length(scenario_sources)) {  # source names match
               
               message("Running predict scenarios...")
               
  # create global variable from list names (JacobResults)
  # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
  # 'Parmnames' list of variable names 
               
               
  for(i in 1:length(estimate.list$JacobResults)){
    tempobj=estimate.list$JacobResults[[i]]
    eval(parse(text=paste(names(estimate.list$JacobResults)[[i]],"= tempobj")))
  }
   # transfer required variables to global environment from SUBDATA
   datalstreq <- data_names$sparrowNames
   for (i in 1:length(datalstreq)) {
     dname <- paste("subdata$",datalstreq[i],sep="")
     x1name <- paste(datalstreq[i],sep="")
     if((x1name %in% names(subdata)) == TRUE) {
       assign(x1name,eval(parse(text=dname)))
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
  
  #############################################
  ############################################
  ###################Rshiny####################
  if (Rshiny==TRUE){
  #break up factors for select reaches
  if (input$domain=="selected reaches"){
    #break apart factors
    #selectFactors<-trimws(input$selectReaches,"both")
    #selectFactors<-substr(selectFactors,3,nchar(selectFactors)-1)
    #selectFactors<-trimws(strsplit(selectFactors,";")[[1]],"both")
    
    #apply source reduction factors to create 'scenario_factor' with selected reach application of factors
    #scenario_factor <- matrix(1,nrow=length(DataMatrix.list$data[,1]),ncol=length(input$scenario_sources))
    for (f in 1:length(input$selectFuncs)){
      #    eval(parse(text=paste("subdata$",as.character(input$scenario_sources[f]),"<-",selectFactors[f],sep="")))
     # eval(parse(text=paste("scenario_factor[,",f,"]","<-",selectFactors[f],sep="")))
    #  eval(parse(text=paste("S_",input$scenario_sources[f],"<-",selectFactors[f],sep="")))
      eval(parse(text = input$selectFuncs[f]))
    }

  }#select reaches
    scenario_sources<-as.character(input$scenario_sources)
    if_predict_scenarios<-as.character(input$domain)  
    if (!dir.exists(paste(path_results,"/scenarios/",input$scenarioName,sep=""))){
      dir.create(paste(path_results,"/scenarios/",input$scenarioName,sep=""),showWarnings = FALSE)
    }
  }#Rshiny TRUE
  ############################################
  ###########################################
  

  ###############################################################################
  # Setup explanatory data matrix based on user-specified load-reduction scenarios


  if((if_predict_scenarios == "all reaches" & Rshiny==FALSE) | (input$domain == "all reaches" & Rshiny==TRUE)) {
  if (Rshiny==TRUE){
   # scenario_all_factors<-eval(parse(text=as.character(input$factors)))  
    scenario_all_factors<-input$scenario_all_factors

  }

  # Adjust source data to reflect uniform user-specified load-reduction scenarios
     for (i in 1:length(srcvar)) {
      for (j in 1:length(scenario_sources)){
       if(srcvar[i] == scenario_sources[j]) {
         if(scenario_all_factors[j] < 1) {

           data[,jsrcvar[i]] <- data[,jsrcvar[i]] * (1-scenario_all_factors[j])

         } else {


           data[,jsrcvar[i]] <- data[,jsrcvar[i]] * scenario_all_factors[j]

         }
       }
      }
     }
  } else {    # apply factors to selected reaches (check for variables with "S_" prefix)
    for (i in 1:length(srcvar)) {
      for (j in 1:length(scenario_sources)){
        if(srcvar[i] == scenario_sources[j]) {
          # transfer values from string name to temp vector
          temp <- eval(parse(text=paste("S_",srcvar[i],sep="")))
          data[,jsrcvar[i]] <- ifelse(temp < 1,(data[,jsrcvar[i]] * (1-temp)),(data[,jsrcvar[i]] * temp))
        }
      }
    }
  }
    # transfer the baseline predictions for load and yield
    predmatrix_base <- predict.list$predmatrix
    yldmatrix_base <- predict.list$yldmatrix
  
  ################################
  # Setup variables for prediction 
  
  nreach <- length(data[,1])
  numsites <- sum(ifelse(data[,10] > 0,1,0))  # jdepvar site load index
  
  # transfer estimated parameters into complete parameter vector (inclusive of non-estimated constants)
  betalst <- oEstimate 

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

    
  # Total decayed load (with monitoring adjustment)

    incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
    carryf <- ifelse(is.na(carryf),0,carryf)
    ifadjust <- 1     # monitoring load adjustment
    
    # Fortran subroutine to accumulate mass climbing down the reach network, compute and accumulate incremental RCHLD
    #   tnoder.dll must be placed in SYSTEM PATH accessible directory
    
    return_data <- .Fortran('ptnoder',
                            ifadjust=as.integer(ifadjust),
                            nreach=as.integer(nreach),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incddsrc=as.double(incddsrc),
                            carryf=as.double(carryf),
                            ee=as.double(ee),PACKAGE="ptnoder") 
    pred <- return_data$ee
    
    mpload_total <- pred  # monitoring-adjusted total load
    
    
  # Total nondecayed load 
    
    incddsrc_nd <- ifelse(is.na(incddsrc_nd),0,incddsrc_nd)
    carryf_nd <- ifelse(is.na(carryf_nd),0,carryf_nd)
    pred <- matrix(0,nrow=nreach,ncol=1)
    ifadjust <- 0     # no monitoring load adjustment
    
    # Fortran subroutine to accumulate mass climbing down the reach network, compute and accumulate incremental RCHLD
    #   tnoder.dll must be placed in SYSTEM PATH accessible directory
    
    return_data <- .Fortran('ptnoder',
                            ifadjust=as.integer(ifadjust),
                            nreach=as.integer(nreach),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incddsrc_nd=as.double(incddsrc_nd),
                            carryf_nd=as.double(carryf_nd),
                            ee=as.double(ee),PACKAGE="ptnoder") 
    pred <- return_data$ee
    
    pload_nd_total <- pred   
    

  # Total load for each SOURCE (decayed and nondecayed)
    
    srclist_total <- character(length(jsrcvar))
    srclist_nd_total <- character(length(jsrcvar))
    srclist_mtotal <- character(length(jsrcvar))
    
  for (j in 1:length(jsrcvar)) { 

    ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 

    # incremental delivered load
    incddsrc <- rchdcayf**0.5 * resdcayf * ddliv
    incddsrc_nd <- ddliv
    
    # Compute the reach transport factor
    carryf <- data[,jfrac] * rchdcayf * resdcayf
    carryf_nd <- data[,jfrac] 

  # Decayed total source load
    pred <- matrix(0,nrow=nreach,ncol=1)
    i_obs <- 1
    
    incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
    carryf <- ifelse(is.na(carryf),0,carryf)
    ifadjust <- 0     # no monitoring load adjustment
    
    return_data <- .Fortran('ptnoder',
                            ifadjust=as.integer(ifadjust),
                            nreach=as.integer(nreach),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incddsrc=as.double(incddsrc),
                            carryf=as.double(carryf),
                            ee=as.double(ee),PACKAGE="ptnoder") 
    pred <- return_data$ee
    
    assign(paste("pload_",Parmnames[j],sep=""),pred)   # create variable 'pload_(source name)'
    srclist_total[j] <- paste("pload_",Parmnames[j],sep="")
    
    assign(paste("mpload_",Parmnames[j],sep=""),pred)   # create variable 'mpload_(source name)'
    srclist_mtotal[j] <- paste("mpload_",Parmnames[j],sep="")
    
  # Nondecayed total source load
    pred <- matrix(0,nrow=nreach,ncol=1)
    i_obs <- 1
    
    incddsrc_nd <- ifelse(is.na(incddsrc_nd),0,incddsrc_nd)
    carryf_nd <- ifelse(is.na(carryf_nd),0,carryf_nd)
    ifadjust <- 0     # no monitoring load adjustment
    
    return_data <- .Fortran('ptnoder',
                            ifadjust=as.integer(ifadjust),
                            nreach=as.integer(nreach),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incddsrc_nd=as.double(incddsrc_nd),
                            carryf_nd=as.double(carryf_nd),
                            ee=as.double(ee),PACKAGE="ptnoder") 
    pred <- return_data$ee
    
    assign(paste("pload_nd_",Parmnames[j],sep=""),pred)   # create variable 'pload_(source name)'
    srclist_nd_total[j] <- paste("pload_nd_",Parmnames[j],sep="")
  } 

    
  # Monitoring-adjusted source loads (computed as shares of nonadjusted total loads)
  if(numsites > 0) {
    srclist_mtotal <- character(length(jsrcvar))
   for (j in 1:length(jsrcvar)) {
    share <- eval(parse(text=srclist_total[j])) / pload_total   # source share of total load
    share <- ifelse(is.na(share),0,share)

    ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
    
    # incremental delivered load
    incddsrc <- rchdcayf**0.5 * resdcayf * ddliv
    incddsrc_nd <- ddliv
    
    # Compute the reach transport factor
    carryf <- data[,jfrac] * rchdcayf * resdcayf
    carryf_nd <- data[,jfrac] 
    
    # Decayed total source load
    pred <- matrix(0,nrow=nreach,ncol=1)
    
    incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
    carryf <- ifelse(is.na(carryf),0,carryf)
    ifadjust <- 1     # monitoring load adjustment
    
    return_data <- .Fortran('mptnoder',
                            ifadjust=as.integer(ifadjust),
                            share=as.double(share),
                            nreach=as.integer(nreach),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incddsrc=as.double(incddsrc),
                            carryf=as.double(carryf),
                            ee=as.double(ee),PACKAGE="mptnoder") 
    pred <- return_data$ee
    
    assign(paste("mpload_",Parmnames[j],sep=""),pred)   # create variable 'mpload_(source name)'
    srclist_mtotal[j] <- paste("mpload_",Parmnames[j],sep="")
   }
  }
    
  # Delivery fraction
    data2 <- matrix(0,nrow=nreach,ncol=5) 
    data2[,1] <- data[,jfnode]
    data2[,2] <- data[,jtnode]
    data2[,3] <- data[,jfrac]
    data2[,4] <- data[,jiftran]
    data2[,5] <- data[,jtarget]    # termflag indicators (=1, =3)

    deliver <- function(incdecay) { 
      sumatt <- matrix(0,nrow=nreach,ncol=1)
      fsumatt <- matrix(0,nrow=nreach,ncol=1)
      return_data <- .Fortran('deliv_fraction',
                              numrchs=as.integer(nreach),
                              waterid=as.integer(waterid),
                              nnode=as.integer(nnode),
                              data2=as.double(data2),
                              incdecay=as.double(incdecay),
                              totdecay=as.double(totdecay),
                              sumatt=as.double(sumatt),PACKAGE="deliv_fraction") 
      fsumatt <- return_data$sumatt
      return(fsumatt) 
    }  # end sumatts function
    deliv_frac <- deliver(incdecay)

  #######################################
  # Output load predictions
    
    srclist_inc_deliv <- character(length(jsrcvar))   # delivered incremental load
    for (i in 1:length(jsrcvar)) {
      srclist_inc_deliv[i] <- paste(srclist_inc[i],"_deliv",sep="")
    }
    
    srclist_inc_share <- character(length(jsrcvar))   # incremental source share (percent)
    for (i in 1:length(jsrcvar)) {
      srclist_inc_share[i] <- paste("share_inc_",srcvar[i],sep="")
    }
      
    srclist_total_share <- character(length(jsrcvar))   # total source share (percent)
    for (i in 1:length(jsrcvar)) {
      srclist_total_share[i] <- paste("share_total_",srcvar[i],sep="")
    }
      
    oparmlist <- c("waterid","pload_total",srclist_total,
    "mpload_total",srclist_mtotal, 
    "pload_nd_total",srclist_nd_total, 
    "pload_inc",srclist_inc, 
    "deliv_frac",
    "pload_inc_deliv",srclist_inc_deliv,
    srclist_total_share,srclist_inc_share) 
    
    # create matrix with predictions
    ncols <- 7 + length(srclist_total) + length(srclist_mtotal) + length(srclist_nd_total) + 
      length(srclist_inc) + length(srclist_inc) + length(srclist_inc) + length(srclist_inc)
    predmatrix <- matrix(0,nrow=length(pload_total),ncol=ncols)
    loadunits <- rep("Kg/Year",ncols)
    
    
    predmatrix[,1] <- subdata$waterid
    # total load
    predmatrix[,2] <- pload_total * bootcorrection
    for (i in 1:length(srclist_total)){
      predmatrix[,2+i] <- eval(parse(text=srclist_total[i])) * bootcorrection
      predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- 
        predmatrix[,2+i] / predmatrix[,2] * 100  # source share
      
      # avoids reporting NAs for share
      predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- 
        ifelse(is.na(predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)]),
               0,predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)]) 
      
        loadunits[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- "Percent"  # source share
    }
    # monitored-adjusted total load
    predmatrix[,(3+length(srclist_total))] <- mpload_total 
    for (i in 1:length(srclist_mtotal)){
      predmatrix[,(3+length(srclist_total)+i)] <- eval(parse(text=srclist_mtotal[i]))
    } 
    # nondecayed (ND) total load
    predmatrix[,(4+length(srclist_total)+length(srclist_mtotal))] <- pload_nd_total * bootcorrection
    for (i in 1:length(srclist_nd_total)){
      predmatrix[,(4+length(srclist_total)+length(srclist_mtotal)+i)] <- eval(parse(text=srclist_nd_total[i])) * bootcorrection
    }
    # incremental load
    predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] <- pload_inc * bootcorrection
    for (i in 1:length(srclist_inc)){
      predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] <- eval(parse(text=srclist_inc[i])) * bootcorrection 
      predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                     length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- 
        predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] / 
        predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] * 100  # source share
      
      # avoids reporting NAs for share
      predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
        length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- 
        ifelse(is.na(predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)]),
          0,predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)])

        loadunits[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                     length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- "Percent"  # source share
    }
    # delivery fraction
    predmatrix[,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- deliv_frac
    loadunits[(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- "Fraction Delivered"
      
    # delivered incremental load
      dload <- predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] * deliv_frac
      predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- dload
      for (i in 1:length(srclist_inc)){
        dload <- predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] * deliv_frac
        predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+i)] <- dload
      }

    
# Output yield predictions
    
    # replace "pload" with "yld" in each string
    srclist_yield <- gsub("pload", "yield", srclist_total)
    srclist_myield <- gsub("pload", "yield", srclist_mtotal)
    srclist_yldinc <- gsub("pload", "yield", srclist_inc)
    srclist_yldinc_deliv <- gsub("pload", "yield", srclist_inc_deliv)
    
    oyieldlist <- c("waterid","concentration","yield_total",srclist_yield,
                   "myield_total",srclist_myield, 
                   "yield_inc",srclist_yldinc, 
                   "yield_inc_deliv",srclist_yldinc_deliv) 
    
    
    ncols <- 6 + length(srclist_yield) + length(srclist_myield) + length(srclist_yldinc) + length(srclist_yldinc_deliv)
    yldmatrix <- matrix(0,nrow=length(pload_total),ncol=ncols)
    yieldunits <- rep("Kg/Km2/Year",ncols)
    
    yldmatrix[,1] <- subdata$waterid
    # total yield
    for (i in 1:length(subdata$waterid)) {
     if(demtarea[i] > 0) {
      if(meanq[i] > 0) { yldmatrix[i,2] <- predmatrix[i,2] / meanq[i] * ConcFactor }   # concentration
      yldmatrix[i,3] <- predmatrix[i,2] / demtarea[i]
      
      for (j in 1:length(srclist_total)){
       yldmatrix[i,3+j] <- predmatrix[i,2+j] / demtarea[i]
      }
    # monitored-adjusted total yield
      yldmatrix[i,(4+length(srclist_total))] <- predmatrix[i,(3+length(srclist_total))] / demtarea[i]
      for (j in 1:length(srclist_mtotal)){
       yldmatrix[i,(4+length(srclist_total)+j)] <- predmatrix[i,(3+length(srclist_total)+j)] / demtarea[i]
      }
     }
    }
    # incremental yield
    for (i in 1:length(subdata$waterid)) {
      if(demiarea[i] > 0) {
        yldmatrix[i,(5+length(srclist_total)+length(srclist_mtotal))] <- predmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] / demiarea[i]
        
        for (j in 1:length(srclist_inc)){
         yldmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+j)] <- predmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+j)] / demiarea[i]
        }
        
    # delivered incremental yield
        yldmatrix[i,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc))] <- 
          predmatrix[i,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] / demiarea[i]
        
        for (j in 1:length(srclist_inc)){
          yldmatrix[i,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc)+j)] <- 
            predmatrix[i,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+j)]  / demiarea[i]
        }
      }
    }
    
    
    predict.source.list <- named.list(srclist_total,srclist_mtotal,srclist_inc,srclist_inc_deliv,
                                      srclist_nd_total,srclist_yield,srclist_myield,srclist_yldinc,
                                      srclist_yldinc_deliv) 
    
    
    # Compute the change from load-reduction scenarios, expressed as ratio of new to baseline load
    predmatrix_chg <- matrix(0,nrow=length(pload_total),ncol=ncol(predmatrix_base))
    yldmatrix_chg <- matrix(0,nrow=length(pload_total),ncol=ncol(yldmatrix_base))
    
    predmatrix_chg[,1] <- predmatrix[,1]
    yldmatrix_chg[,1] <- yldmatrix[,1]
    for (i in 2:ncol(predmatrix_base)) {
      temp <- predmatrix[,i] / predmatrix_base[,i]
      temp <- ifelse(is.na(temp),0,temp)
      predmatrix_chg[,i] <- temp
    }
    for (i in 2:ncol(yldmatrix_base)) {
      temp <- yldmatrix[,i] / yldmatrix_base[,i]
      temp <- ifelse(is.na(temp),0,temp)
      yldmatrix_chg[,i] <- temp
    }
    
  predictScenarios.list <- named.list(if_predict_scenarios,scenario_sources,scenario_all_factors,
                                      oparmlist,loadunits,predmatrix,oyieldlist,yieldunits,yldmatrix,
                                      predict.source.list,predmatrix_chg,yldmatrix_chg)
  
  assign("predictScenarios.list",predictScenarios.list, envir=.GlobalEnv)
  
  #########################################
  #########################################
  ##################end predictScenarios#######

  #output csv files
     predictScenariosOutCSV(#Rshiny 
                            input, Rshiny,
                            #regular
                            path_results,file_sum,estimate.list,predictScenarios.list,subdata,add_vars,
                            scenario_name,data_names,csv_decimalSeparator, csv_columnSeparator,
                            batch_mode,ErrorOccured) 

  ###########Start predictStreamMapsScenarios###
 # if((!is.na(scenario_map_list[1]) & Rshiny==FALSE) | Rshiny==TRUE) { # from controlfiletasksmodel
   # if (Rshiny==FALSE | (Rshiny==TRUE & input$batch=="Batch")){
  if(!is.na(scenario_map_list[1]) & Rshiny==FALSE){  
 # if (Rshiny==FALSE){
    message("Running scenario mapping in batch mode...")
mapScenarios<-TRUE

    dir.create(paste(path_results,"/maps/batch",sep=""))
    save(list = c(as.character(outputSettings(path_results,file_sum,csv_decimalSeparator,csv_columnSeparator,FALSE,batch_mode,ErrorOccured)$setting),
                  "ErrorOccured","runScript","run2",ls()[which(regexpr("path_",ls())>0)],"path_gis",
                  ls()[which(regexpr("file_",ls())>0)],"estimate.input.list","mapping.input.list","Rshiny","allMetrics",
                  "mapScenarios","predictScenarios.list"),
         file=paste(path_main,"/batch/batch.RData",sep=""))
    
    save(list=c("data_names"),
         file=paste(path_results,"/maps/batch/batch.RData",sep=""),compress = FALSE)
    
    system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,"/batch/batchMaps.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
    
    
    unlink(paste(path_results,"/maps/batch",sep=""),recursive=TRUE)

    }else{
      mapScenarios<-TRUE
      
      
      if (input$batch!="Batch"){
      output_map_type<-tolower(c(trimws(gsub("-","",input$outType))))
      }

      predictMaps(#Rshiny
        input, allMetrics, output_map_type,Rshiny,
        #regular
        path_results,file_sum,path_gis,
        #map_uncertainties,BootUncertainties,
        data_names,mapping.input.list,
        #predict.list,
        subdata,
        #scenarios
        mapScenarios,
        scenario_map_list,
        predictScenarios.list,
        scenario_name,
        batch_mode,ErrorOccured)
    }
   # }#!is.na(scenario_map_list[1]
  
  #############################################
  #############################################
  
             }  else { # check on variable names; names do not match
               message(' \nWARNING : No scenarios executed because source variables are not correct in scenario_sources setting\n ')
               if (batch_mode=="yes"){
                 cat(' \nWARNING : No scenarios executed because source variables are not correct in scenario_sources setting\n ')
               }
             }#conditions from controlfiletaskmodel
       
             }#initial condition 
       
       }

       },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("predictScenarios.R",batch_mode)
       }
     }else{#if no error
     # return(predictScenarios.list) 
     }#end if error
     
   }#test if previous error
 }#end function
    
