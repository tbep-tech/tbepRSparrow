#'@title verifyDemtarea
#'@description verify demtarea and output results as csv and plot
#'@param if_verify_demtarea specify whether or not to verify demtarea
#'@param data1 data1 object
#'@param compareData data with new demtarea values (contains waterid, hydseq, demtarea, and headflag)
#'@param path_results path to model subdirectory
#'@param file_sum run_id for current model
#'@param path_gis path to gis directory
#'@param mapping.input.list mapping.input.list from control file
#'@param csv_decimalSeparator decimal separator for csv output
#'@param csv_columnSeparator column separator for csv output
#'@param batch_mode yes/no indicating whether or not batch processing is used
#'@param ErrorOccured yes/no indicating if a previous error has occured `ErrorOccured<-"no"`

verifyDemtarea<-function(if_verify_demtarea,data1,compareData,
                         #for checkDrainageErrors
                         path_results,file_sum,path_gis,mapping.input.list,
                         csv_decimalSeparator, csv_columnSeparator,
                         #for both
                         batch_mode, ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  if (if_verify_demtarea=="yes"){
    
  # Select reaches to be included in the analysis (exclude coastal shorelines)
  # NAs removed first or will create NA records in 'sub1'
  for (c in c("termflag","fnode","tnode","demiarea","demtarea")){
    eval(parse(text = paste("data1$",c,"<-ifelse(is.na(data1$",c,"),0,data1$",c,")",sep="")))
  }
  
  sub1 <- data1[(data1$fnode > 0 & data1$tnode > 0 & data1$termflag != 3), ]
  
  #get compareData as vectors
  waterid<-compareData$waterid
  hydseq<-compareData$hydseq
  demtarea<-compareData$demtarea
  headflag<-compareData$headflag
  
  #verifyDemtarea

    demtarea_new <- demtarea
    hydseq_new <- hydseq
    headflag_new <- headflag
    name1 <- paste("hs_data <- data.frame(waterid,demtarea_new,hydseq_new,headflag_new)",sep="")
    eval(parse(text=name1))
    hs_data <- hs_data[hs_data$waterid != 0, ] # eliminate 0 cases where vector dimension max > no. reaches
    sub1 <- merge(sub1,hs_data,by="waterid",all.y=TRUE,all.x=TRUE)
    sub1.plot <- qplot(sub1$demtarea,sub1$demtarea_new,log="xy",
                     xlab="Pre-calculated Total Drainage Area",ylab="Newly-calculated Total Drainage Area",
                     geom=c("point"),main="Comparison of Total Drainage Areas")

    
    sub1$AreaRatio_NewOld <- sub1$demtarea_new / sub1$demtarea
    sub1$Headflag_NewOld <- as.character(ifelse(sub1$headflag == sub1$headflag_new,"  ","DIFFER"))
    sub1$AreaRatio_NewOld[is.na(sub1$AreaRatio_NewOld)] <- 0     # NAs removed first or will create NA records in 'sub1'
    DAreaFailCheckObj <- sub1[(sub1$AreaRatio_NewOld < 0.99 | sub1$AreaRatio_NewOld > 1.01), ]
    if(nrow(DAreaFailCheckObj) > 0) {
      DAreaFailCheckMessage <- 
        paste("Number of reaches with different (>1%) total drainage areas (see records in DAreaFailCheckObj): ",
              nrow(DAreaFailCheckObj),sep="")
      
    }
    
    #output results
    if (DAreaFailCheckMessage!=" "){
      message("Reporting checks of total drainage area...")
      message(DAreaFailCheckMessage)
      cat("\n\n")
      if (batch_mode=="yes"){
        cat(DAreaFailCheckMessage)
        cat("\n\n") 
      }
    } else {
      message("No errors found in checks of total drainage area. Area differences are <1%.")
    }
    if(DAreaFailCheckMessage!=" ") {    # Map mis-matched reaches and diagnostics
      if(nrow(DAreaFailCheckObj) > 0) {
        if (ErrorOccured=="no"){
          message("Writing results from drainage area comparisons (CSV, PDF maps) in estimate directory...")
        }#if no error
        checkDrainageareaErrors(path_results,file_sum,path_gis,mapping.input.list,
                                sub1.plot,DAreaFailCheckObj,data1, csv_decimalSeparator, csv_columnSeparator,
                                batch_mode,ErrorOccured)
      }
    }
    
  }#end if_verify 

  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("verifyDemtarea.R",batch_mode)
      }
    }else{#if no error

    }#end if error
    
  }#test if previous error
  
  
}#end function