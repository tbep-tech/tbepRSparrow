#
# predictOutCSV.R
#
#####################################################################################################
# Calculates predictions of load

 predictBootsOutCSV <- function(path_results,file_sum,estimate.list,predictBoots.list,subdata,add_vars,
                                data_names,csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured) {
  
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

# create global variable from list names (JacobResults)
# 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
# 'Parmnames' list of variable names 
  for(i in 1:length(estimate.list$JacobResults)){
    tempobj=estimate.list$JacobResults[[i]]
    eval(parse(text=paste(names(estimate.list$JacobResults)[[i]],"= tempobj")))
  }

  # create global variable from list names (predictBoots.list)
  for(i in 1:length(predictBoots.list)){
    tempobj=predictBoots.list[[i]]
    eval(parse(text=paste(names(predictBoots.list)[[i]],"= tempobj")))
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

  #test if waterid was renumbered, if so add it to add_vars
  origWaterid<-as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)
  if (origWaterid!="waterid"){
    add_vars<-c(origWaterid,add_vars)    
  }else{
    add_vars<-c("waterid_for_RSPARROW_mapping",add_vars)
  }
  
  #get user selected additional variables (add_vars)
  add_vars<-add_vars[which(add_vars!="waterid")]
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
          addSubdataVars<-cbind(addSubdataVars,data.frame(temp = subdata[,which(names(subdata) %in% tempCol)]))
          names(addSubdataVars)[2]<-tempName
        }else{
          addSubdataVarstemp<-data.frame(temp = subdata[,which(names(subdata) %in% tempCol)])
          names(addSubdataVarstemp)[1]<-tempName
          addSubdataVars<-cbind(addSubdataVars,addSubdataVarstemp)
        } 
      }# a == data1UserNames where sparrowName = waterid
      
    }#for a in add_vars
  }  

  # Output load predictions
  outvars <- as.data.frame(bootmatrix)
  colnames(outvars) <- boparmlist  
  
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
  
  outvars2 <- outvars2[with(outvars2,order(outvars2$hydseq,outvars2$waterid)), ]
  
  fileout <- paste(path_results,"/predict/",file_sum,"_predicts_load_boots.csv",sep="")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")

  # Output yield predictions
  outvars <- as.data.frame(bootyldmatrix)
  colnames(outvars) <- byldoparmlist  
  
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
  outvars2 <- outvars2[with(outvars2,order(outvars2$hydseq,outvars2$waterid)), ]
  
  fileout <- paste(path_results,"/predict/",file_sum,"_predicts_yield_boots.csv",sep="")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("predictBootsOutCSV.R",batch_mode)
       }
     }else{#if no error

     }#end if error
     
   }#test if previous error
 }#end function
    
