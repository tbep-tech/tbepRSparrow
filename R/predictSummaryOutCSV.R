#
# predictOutCSV.R
#
#####################################################################################################
# Calculates percential summaries of predictions of yield, concentration, and delivery fraction

 predictSummaryOutCSV <- function(path_results,file_sum,ConcFactor,yieldFactor,ConcUnits,yieldUnits,
                                  SelParmValues,estimate.list,predict.list,
                                  subdata,class_landuse, class_landuse_percent,
                                  csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured) {
  
  # INPUT objects:
  # path_results
  # file_sum
  # SelParmValues
  # estimate.list
  # predict.list

# Summary loads:
#   deliv_frac                 Fraction of total load delivered to terminal reach
#   share_inc_(sources)        Source share for incremental load (percent)
# Summary yields:
#   Concentration              Concentration based on decayed total load and discharge
#   yield_total                Total yield (fully decayed)
#   yield_inc                  Total incremental yield delivered to streams

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

  # create global variable from list names
  for(i in 1:length(predict.list)){
    tempobj=predict.list[[i]]
    eval(parse(text=paste(names(predict.list)[[i]],"= tempobj")))
  }

   # create global variable from SelParmValues
   for(i in 1:length(SelParmValues)){
     tempobj=SelParmValues[[i]]
     eval(parse(text=paste(names(SelParmValues)[[i]],"= tempobj")))
   }

   
   
   # Transfer loads to matrix and compute summary percentiles
   omatrix <- matrix(0,nrow=nrow(predmatrix),ncol=(4+length(srcvar)))
   
   #   deliv_frac                 Fraction of total load delivered to terminal reach
   for (i in 1:length(oparmlist)) {
     if(oparmlist[i] == "deliv_frac"){
       omatrix[,4] <- predmatrix[,i]
     }
   }
   
   #   Concentration              Concentration based on decayed total load and discharge
   for (i in 1:length(oyieldlist)) {
     if(oyieldlist[i] == "concentration"){
       omatrix[,3] <- yldmatrix[,i]
     }
   }
   
   #   yield_total                Total yield (fully decayed)
   for (i in 1:length(oyieldlist)) {
     if(oyieldlist[i] == "yield_total"){
       omatrix[,1] <- yldmatrix[,i] 
     }
   }
   
   #   yield_inc                  Total incremental yield delivered to streams
   for (i in 1:length(oyieldlist)) {
     if(oyieldlist[i] == "yield_inc"){
       omatrix[,2] <- yldmatrix[,i]
     }
   }
   
   #   share_inc_(sources)        Source share for incremental load (percent)
   for (j in 1:length(srcvar)){
     dname <- paste("share_inc_",srcvar[j],sep="")  
     for (i in 1:length(oparmlist)) {
       if(oparmlist[i] == dname){
         omatrix[,(4+j)] <- predmatrix[,i]
       }
     }
   }
   name1 <- paste("Yield Total (",yieldUnits,")",sep="")
   name2 <- paste("Yield Incremental (",yieldUnits,")",sep="")
   name3 <- paste("Flow-Weighted Concentration (",ConcUnits,")",sep="")
   sumlist <- c(name1,name2,name3,"Delivery Fraction")
   for (i in 1:length(srcvar)){
     sumlist[4+i] <- paste("Incremental Share (%) ",srcvar[i],sep="")  
   }
   
   # Compute percentiles
   sstats <- matrix(0,nrow=(4+length(srcvar)),ncol=9)
   for (i in 1:(4+length(srcvar))){
     sstats[i,] <- quantile(round(omatrix[,i],digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))
   }
   mstats <- matrix(0,nrow=(4+length(srcvar)),12)
   for (i in 1:(4+length(srcvar))){
     mstats[i,1] <- nrow(omatrix)
     mstats[i,2] <- round(mean(omatrix[,i]),digits=3)
     mstats[i,3] <- round(sd(omatrix[,i]),digits=3)
   }
   for (i in 4:12) {
     mstats[,i] <- sstats[,i-3]
   }
   headlist <- c("Number Watersheds","Mean","Standard Deviation","2.5th","10th","20th","30th","50th",
                 "70th","80th","90th","97th")

  # Output summary predictions
    # prep for output to CSV
    outvars <- as.data.frame(mstats,sstats)
    rownames(outvars) <- sumlist
    colnames(outvars) <- headlist
    
    #calculate yields for user selected class_landuse_percent
    if (!is.na(class_landuse_percent) & !is.na(class_landuse)){
      classLandusePercent<-calcClassLandusePercent(subdata,class_landuse, class_landuse_percent,batch_mode,ErrorOccured)
      #for each class_landuse cget yield add to outvars
      for (c in class_landuse){
        subWaterid<-classLandusePercent[which(classLandusePercent$landuse==c),]$waterid
        subYield<-yldmatrix[yldmatrix[,1] %in% subWaterid,]
        
        # Compute percentiles
          sstats<- matrix(quantile(round(subYield[,3],digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)),nrow=1)
          mstats <- matrix(0,nrow=1,ncol = 3)
          mstats[1,1] <- nrow(subYield)
          mstats[1,2] <- round(mean(subYield[,3]),digits=3)
          mstats[1,3] <- round(sd(subYield[,3]),digits=3)

        headlist <- c("Number Watersheds","Mean","Standard Deviation","2.5th","10th","20th","30th","50th",
                      "70th","80th","90th","97th")
        
        #make dataframe
        LUoutvars <- as.data.frame(matrix(c(mstats,sstats),nrow=1))
        rownames(LUoutvars) <- paste(class_landuse_percent[which(class_landuse==c)],"%",c,"_",name1,sep="")
        colnames(LUoutvars) <- headlist 
        #bind to outvars
        outvars<-rbind(outvars,LUoutvars)

      }#end for each class_landuse
    }#end if class_landuse_percent
    
    fileout <- paste(path_results,"/estimate/",file_sum,"_summary_predictions.csv",sep="")
    fwrite(outvars,file=fileout,row.names=T,append=F,quote=F,col.names=TRUE,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
    
     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("predictSummaryOutCSV.R",batch_mode)
       }
     }else{#if no error
       
     }#end if error
     
   }#test if previous error
 }#end function
    
