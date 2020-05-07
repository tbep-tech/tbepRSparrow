#
# predictScenariosOutCSV.R
#
#####################################################################################################
# Calculates predictions of load

 predictScenariosOutCSV <- function(#Rshiny 
                                    input, Rshiny,
                                   #regular
                                    path_results,file_sum,estimate.list,predictScenarios.list,subdata,add_vars,
                                    scenario_name,data_names,csv_decimalSeparator, csv_columnSeparator,
                                    batch_mode,ErrorOccured) {
 #Rshiny input
   #input$domain
   #input$scenario_sources
   #input$factors
    
  # INPUT objects:
  # path_results
  # file_sum
  # estimate.list
  # DataMatrix.list
  # SelParmValues
  # reach_decay_specification
  # reservoir_decay_specification
  # subdata
  # bootcorrection

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

       if (Rshiny==TRUE){
         scenario_name<-input$scenarioName
       }
       
# create global variable from list names (JacobResults)
# 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
# 'Parmnames' list of variable names 
  for(i in 1:length(estimate.list$JacobResults)){
    tempobj=estimate.list$JacobResults[[i]]
    eval(parse(text=paste(names(estimate.list$JacobResults)[[i]],"= tempobj")))
  }

  # create global variable from list names
  for(i in 1:length(predictScenarios.list)){
    tempobj=predictScenarios.list[[i]]
    eval(parse(text=paste(names(predictScenarios.list)[[i]],"= tempobj")))
  }

  # transfer required variables to global environment from SUBDATA
  datalstCheck <-  as.character(c(getVarList()$varList,"waterid_for_RSPARROW_mapping"))
  for (i in 1:length(datalstCheck)) {
    dname <- paste("subdata$",datalstCheck[i],sep="") 
    x1name <- paste(datalstCheck[i],sep="")
    if((x1name %in% names(subdata)) == TRUE) {
      assign(datalstCheck[i],eval(parse(text=dname)))
    }
  }
  
  #test if waterid was renumbered, if so add it to add_vars
  origWaterid<-as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)
  if (unique(unique(waterid_for_RSPARROW_mapping-waterid)!=0) & length(unique(unique(waterid_for_RSPARROW_mapping-waterid)!=0))==1){
    add_vars<-c("waterid_for_RSPARROW_mapping",add_vars)
  }else if (origWaterid!="waterid"){
    add_vars<-c(origWaterid,add_vars)
  }
  
  #get user selected additional variables (add_vars)
  add_vars<-c("waterid",add_vars)
  add_vars<-add_vars[!duplicated(add_vars)]
  if (length(na.omit(add_vars))!=0){
    for (a in na.omit(add_vars)){
      if (a!=as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames) & a!="waterid_for_RSPARROW_mapping"){
        if (a %in% names(subdata)){
          if (a==add_vars[1]){#if a is first add_var
            addSubdataVars<-subdata[,which(names(subdata) %in% c("waterid",a))] 
          }else{
            addSubdataVarstemp<-data.frame(temp = subdata[,which(names(subdata) %in% a)])
            names(addSubdataVarstemp)<-a
            addSubdataVars<-cbind(addSubdataVars,addSubdataVarstemp)
          }
        }#if a in subdata
      }else{# a == data1UserNames where sparrowName = waterid |a=="waterid_for_RSPARROW_mapping"
        if (a=="waterid_for_RSPARROW_mapping"){
          tempName<-"originalWaterid"
          tempCol<-"waterid_for_RSPARROW_mapping"
        }else{
          tempName<-as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)
          tempCol<-"waterid"
        }
        if (a==add_vars[1]){#if a is first add_var
          addSubdataVars<-data.frame(waterid = subdata[,which(names(subdata) %in% tempCol)])
          addSubdataVars<-data.frame(waterid = subdata[,which(names(subdata) %in% tempCol)],
                                     temp = subdata[,which(names(subdata) %in% tempCol)])
         # addSubdataVars<-cbind(addSubdataVars,data.frame(temp = subdata[,which(names(subdata) %in% tempCol)]))
          names(addSubdataVars)[2]<-tempName
        }else{
          addSubdataVarstemp<-data.frame(temp = subdata[,which(names(subdata) %in% tempCol)])
          names(addSubdataVarstemp)[1]<-tempName
          addSubdataVars<-cbind(addSubdataVars,addSubdataVarstemp)
        } 
      }# a == data1UserNames where sparrowName = waterid
      
    }#for a in add_vars
    names(addSubdataVars)[1]<-"waterid"
  }
  addSubdataVars<-subset(addSubdataVars, select=which(!duplicated(names(addSubdataVars)))) 
# Output load predictions with changes from load-reduction scenarios
    # prep for output to CSV
    outvars <- as.data.frame(predmatrix)
    colnames(outvars) <- oparmlist  
    
    rchname <- subdata$rchname
    rchname <- gsub(",", "", rchname)
    predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                           demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid)
    outvars2 <- merge(predatts,outvars,by="waterid",all.y=TRUE,all.x=TRUE)
    if (length(na.omit(add_vars))!=0){
      outvars2 <-merge(outvars2,addSubdataVars,by="waterid",all.x=TRUE,all.y=TRUE)
      
      #test if origWaterid column is in addsubdataVars, if so reorder columns so that origWaterid is in column 2
      origWaterid<-names(addSubdataVars)[which(names(addSubdataVars) %in% c("originalWaterid",as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)) 
                                               & names(addSubdataVars)!="waterid")]
      if (length(origWaterid)!=0){
        outvars2<-outvars2[,match(c("waterid",origWaterid,names(outvars2)[which(!names(outvars2) %in% c("waterid",origWaterid))]),names(outvars2))]
      }
        
    }# if add_vars
    outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]  # sort by waterid
    
    fileout <- paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,"_predicts_load_scenario.csv",sep="")
    fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")

# Output load prediction changes (percent) from load-reduction scenarios
    outvars <- as.data.frame(predmatrix_chg)
    colnames(outvars) <- oparmlist  
    
    rchname <- subdata$rchname
    rchname <- gsub(",", "", rchname)
    predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                           demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid)
    outvars2 <- merge(predatts,outvars,by="waterid",all.y=TRUE,all.x=TRUE)
    if (length(na.omit(add_vars))!=0){
      outvars2 <-merge(outvars2,addSubdataVars,by="waterid",all.x=TRUE,all.y=TRUE)
      
      #test if origWaterid column is in addsubdataVars, if so reorder columns so that origWaterid is in column 2
      origWaterid<-names(addSubdataVars)[which(names(addSubdataVars) %in% c("originalWaterid",as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)) 
                                               & names(addSubdataVars)!="waterid")]
      if (length(origWaterid)!=0){
        outvars2<-outvars2[,match(c("waterid",origWaterid,names(outvars2)[which(!names(outvars2) %in% c("waterid",origWaterid))]),names(outvars2))]
      }
    }#if add_vars
    outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]  # sort by waterid
    
    fileout <- paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,"_predicts_loadchg_scenario.csv",sep="")
    fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")

    
# Output yield predictions with changes from load-reduction scenarios
    # prep for output to CSV
    outvars <- as.data.frame(yldmatrix)
    colnames(outvars) <- oyieldlist  
    
    rchname <- subdata$rchname
    rchname <- gsub(",", "", rchname)
    predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                           demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid)
    outvars2 <- merge(predatts,outvars,by="waterid",all.y=TRUE,all.x=TRUE)
    if (length(na.omit(add_vars))!=0){
      outvars2 <-merge(outvars2,addSubdataVars,by="waterid",all.x=TRUE,all.y=TRUE)
      
      #test if origWaterid column is in addsubdataVars, if so reorder columns so that origWaterid is in column 2
      origWaterid<-names(addSubdataVars)[which(names(addSubdataVars) %in% c("originalWaterid",as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)) 
                                               & names(addSubdataVars)!="waterid")]
      if (length(origWaterid)!=0){
        outvars2<-outvars2[,match(c("waterid",origWaterid,names(outvars2)[which(!names(outvars2) %in% c("waterid",origWaterid))]),names(outvars2))]
      }
    }#if add_vars
    outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]    # sort by waterid
    
    fileout <- paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,"_predicts_yield_scenario.csv",sep="")
    fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    
# Output yield prediction changes (percent) from load-reduction scenarios
    outvars <- as.data.frame(yldmatrix_chg)
    colnames(outvars) <- oyieldlist  
    
    rchname <- subdata$rchname
    rchname <- gsub(",", "", rchname)
    predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                           demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid)
    outvars2 <- merge(predatts,outvars,by="waterid",all.y=TRUE,all.x=TRUE)
    if (length(na.omit(add_vars))!=0){
      outvars2 <-merge(outvars2,addSubdataVars,by="waterid",all.x=TRUE,all.y=TRUE)
      
      #test if origWaterid column is in addsubdataVars, if so reorder columns so that origWaterid is in column 2
      origWaterid<-names(addSubdataVars)[which(names(addSubdataVars) %in% c("originalWaterid",as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)) 
                                               & names(addSubdataVars)!="waterid")]
      if (length(origWaterid)!=0){
        outvars2<-outvars2[,match(c("waterid",origWaterid,names(outvars2)[which(!names(outvars2) %in% c("waterid",origWaterid))]),names(outvars2))]
      }
    }#if add_vars
    outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]    # sort by waterid
    
    fileout <- paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,"_predicts_yieldchg_scenario.csv",sep="")
    fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    
# Output meta data to text file documenting settings for scenario
    if (Rshiny==FALSE){
    outvars2 <- data.frame(if_predict_scenarios,scenario_sources,scenario_all_factors)
    }else{
      outvars2 <- data.frame(if_predict_scenarios=as.character(input$domain),
                             scenario_sources=paste(as.character(input$scenario_sources),collapse=","),
                             scenario_all_factors=paste(as.character(input$scenario_all_factors),collapse=","))
    }
    colnames(outvars2) <- c("if_predict_scenarios","scenario_sources","scenario_all_factors")
    
    fileout <- paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,"_scenario_metainfo.txt",sep="")
    fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    
    if (Rshiny==FALSE){
    #save the modifySubdata routine with a record of the source change settings
    filesList<-c("_modifySubdata.R")
    sapply(filesList, function(x) file.copy(paste(path_results,file_sum,x,sep=""),
                                            paste(path_results,"/scenarios/",scenario_name,"/",scenario_name,"_",file_sum,x,sep="")))
    }
    
     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("predictScenariosOutCSV.R",batch_mode)
       }
     }else{#if no error

     }#end if error
     
   }#test if previous error
 }#end function
    
