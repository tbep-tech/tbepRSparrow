#'@title createVerifyReachAttr
#'@description calculates reach attributes using hydseq.R, calcHeadflag.R, and accumulateIncrArea.R.
#'Also verifies demtarea if_verify_demtarea<-"yes"
#'attributes are calculated for fnode>0, tnode>0 and termflag!=3
#'Uses subroutines: hydseq, calcHeadflag, accumulateIncrArea, verifyDemtarea, errorOccurred.
#'@param if_verify_demtarea specify whether or not to verify demtarea
#'@param calculate_reach_attribute_list list of attributes to calculate
#'@param data1 input data (data1) 
#'@param path_results path to results directory
#'@param file_sum user specified run_id 
#'@param path_gis path to users gis data
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit,lon_limit,master_map_list,
#'lineShapeName,lineWaterid,polyShapeName,ployWaterid,LineShapeGeo, LineShapeGeo,CRStext,convertShapeToBinary.list,
#'map_siteAttributes.list,residual_map_breakpoints,site_mapPointScale, if_verify_demtarea_maps 
#'@param csv_decimalSeparator decimal separator for csv output
#'@param csv_columnSeparator column separator for csv output
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return data1 with calculated reach attributes replaced


createVerifyReachAttr <- function(if_verify_demtarea,calculate_reach_attribute_list,data1,
                             path_results,file_sum,path_gis,mapping.input.list,
                             csv_decimalSeparator, csv_columnSeparator,
                             batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({
 
      
  # Select reaches to be included in the analysis (exclude coastal shorelines)
      # NAs removed first or will create NA records in 'sub1'
  for (c in c("termflag","fnode","tnode","demiarea","demtarea")){
    eval(parse(text = paste("data1$",c,"<-ifelse(is.na(data1$",c,"),0,data1$",c,")",sep="")))
  }
  
  sub1 <- data1[(data1$fnode > 0 & data1$tnode > 0 & data1$termflag != 3), ]

  if(!is.na(calculate_reach_attribute_list) == TRUE ){  
   
  if (nrow(sub1)==0){
    ErrorOccured=="yes"
    assign("ErrorOccured","yes",envir = .GlobalEnv)
    assign("ErrorOccured","yes",envir = parent.frame())
    
    cat("\n \n")
    message(paste("HYDSEQ VARIABLE CANNOT BE CALCULATED\n DATASET WITH FNODE>0, TNODE>0 and TERMFLAG!=0 IS EMPTY\nRUN EXECUTION TERMINATED.",sep=""))
    if (batch_mode=="yes"){#if batch output message to log
      cat("HYDSEQ VARIABLE CANNOT BE CALCULATED\n DATASET WITH FNODE>0, TNODE>0 and TERMFLAG!=0 IS EMPTY\nRUN EXECUTION TERMINATED.",sep="")
    }
  }
  if (ErrorOccured=="no"){  
  nreach <- length(sub1$waterid)
 

  #calculate reach attributes if on the list
  if (length(grep("hydseq",calculate_reach_attribute_list))!=0){ 

    #calculate hydseq variable, also headflag and demtarea if called for
      hydseq_data <- hydseq(sub1,calculate_reach_attribute_list, batch_mode,ErrorOccured)

 waterid <- hydseq_data$waterid
 hydseq <- hydseq_data$hydseq
 headflag <- hydseq_data$headflag
 demtarea <- hydseq_data$demtarea
 
 #calculate termflag if called for
 #if (length(grep("termflag",calculate_reach_attribute_list))!=0){
#   termflag_new<-calcTermflag(sub1, batch_mode,ErrorOccured)
#   termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
#   termflag<-termflag_new$termflag
# }else{
#   termflag<-data1$termflag
# }
  
 }else if (length(grep("demtarea",calculate_reach_attribute_list))!=0){
    waterid<-sub1$waterid
    hydseq<-sub1$hydseq
    
    #calculate headflag if called for
    if (length(grep("headflag",calculate_reach_attribute_list))!=0){
      headflag_new<-calcHeadflag(sub1, batch_mode,ErrorOccured)
      headflag_new<-headflag_new[match(sub1$waterid,headflag_new$waterid),]
    headflag<-headflag_new$headflag
    }else{
      headflag<-data1$headflag
    }
    
    #calculate termflag if called for
   # if (length(grep("termflag",calculate_reach_attribute_list))!=0){
  #    termflag_new<-calcTermflag(sub1, batch_mode,ErrorOccured)
  #    termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
  ##    termflag<-termflag_new$termflag
  #  }else{
  #    termflag<-data1$termflag
  #  }
    
    #calculate demtarea  
    demtarea_new<-accumulateIncrArea(sub1,c("demiarea"),c("demtarea"),batch_mode,ErrorOccured)
      demtarea_new<-demtarea_new[match(sub1$waterid,demtarea_new$waterid),]
      demtarea<-demtarea_new$demtarea
 
      }else if (length(grep("headflag",calculate_reach_attribute_list))!=0){
        waterid<-sub1$waterid
        hydseq<-sub1$hydseq
        demtarea<-sub1$demtarea
        
        #calculate headflag
        headflag_new<-calcHeadflag(sub1, batch_mode,ErrorOccured)
        headflag_new<-headflag_new[match(sub1$waterid,headflag_new$waterid),]
        headflag<-headflag_new$headflag
        
        #calculate termflag if called for
     #   if (length(grep("termflag",calculate_reach_attribute_list))!=0){
    #      termflag_new<-calcTermflag(sub1, batch_mode,ErrorOccured)
    #      termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
    #      termflag<-termflag_new$termflag
    #    }else{
    #      termflag<-data1$termflag
    #    }
        
      }else if (length(grep("termflag",calculate_reach_attribute_list))!=0){
        waterid<-sub1$waterid
        hydseq<-sub1$hydseq
        demtarea<-sub1$demtarea
        
        #calculate termflag
         # termflag_new<-calcTermflag(sub1, batch_mode,ErrorOccured)
        #  termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
        #  termflag<-termflag_new$termflag
      }
  
#verifyDemtarea
  compareData<-data.frame(waterid=waterid,
                          hydseq=hydseq,
                          headflag=headflag,
                          demtarea=demtarea)
  
  verifyDemtarea(if_verify_demtarea,sub1,compareData,
                           #for checkDrainageErrors
                           path_results,file_sum,path_gis,mapping.input.list,
                           csv_decimalSeparator, csv_columnSeparator,
                           #for both
                           batch_mode, ErrorOccured)

#replace reach attributes   
 # remove attributes from data1
   drops <- calculate_reach_attribute_list
   data1 <- data1[ , !(names(data1) %in% drops)]
 
 # add attributes to data1
   for (i in 1:length(calculate_reach_attribute_list)) {
     name1 <- paste("hs_data <- data.frame(waterid,",calculate_reach_attribute_list[i],")",sep="")
     eval(parse(text=name1))
     hs_data <- hs_data[hs_data$waterid != 0, ] # eliminate 0 cases where vector dimension max > no. reaches
     data1 <- merge(data1,hs_data,by="waterid",all.y=TRUE,all.x=TRUE)
   }

 

}#if no error

  }else{#if no calc_reach_attr
  if (if_verify_demtarea=="yes"){
    waterid <- sub1$waterid
    hydseq <- sub1$hydseq
    headflag <- sub1$headflag
    
    #calculate demtarea  
    demtarea_new<-accumulateIncrArea(sub1,c("demiarea"),c("demtarea"),batch_mode,ErrorOccured)
    demtarea_new<-demtarea_new[match(sub1$waterid,demtarea_new$waterid),]
    demtarea<-demtarea_new$demtarea
    
    #verifyDemtarea
    compareData<-data.frame(waterid=waterid,
                            hydseq=hydseq,
                            headflag=headflag,
                            demtarea=demtarea)
    
    verifyDemtarea(if_verify_demtarea,sub1,compareData,
                   #for checkDrainageErrors
                   path_results,file_sum,path_gis,mapping.input.list,
                   csv_decimalSeparator, csv_columnSeparator,
                   #for both
                   batch_mode, ErrorOccured)
    
  }#end if_verify_demtarea
}#end if no calc reach attr
      
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("createVerifyReachAttr.R",batch_mode)
      }
    }else{#if no error
      if (ErrorOccured=="no"){
 #return(rchdata.list)
        return(data1)
      }
    }#end if error
    
  }#test if previous error
}#end function




