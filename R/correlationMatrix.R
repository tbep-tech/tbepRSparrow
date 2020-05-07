#'@title correlationMatrix
#'@description Execute summary metrics and bivariate correlations between user-selected variables in subdata for incremental areas of sites and for all reaches
#'Uses subroutines: named.list, errorOccurred.
#'@param path_results path to results directory
#'@param file_sum user specified run_id 
#'@param maxsample is the maximum number of subsamples for the plots
#'@param names is the list of parameters to calculate correlations
#'@param ntype is the parameter type (e.g., SOURCE)
#'@param SelParmValues selected parameters from parameters.csv using condition `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`
#'@param subdata input data (subdata) 
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return named list Cor.ExplanVars.list
#
# correlationMatrix.R
#
##############################################
# Execute bivariate correlations between user-selected variables in subdata

correlationMatrix <- function(path_results,file_sum,maxsamples,names,ntype,SelParmValues,subdata,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 
      
      #### Add libraries to the DESCRIPTION file, but necessary to invoke in function
      library(car)     # scatterplotMatrix function
      library(dplyr)   # sample_n function (requires 'rlang' library)
      
      # create global variables from list names
      rows <- length(assign(names[1],eval(parse(text=paste("subdata$",names[1],sep="")))))
      cmatrix <- matrix(0,nrow=rows,ncol=length(names))
      for(i in 1:length(names)) {
        dname <- paste("subdata$",names[i],sep="")
        xx <- eval(parse(text=dname))
        cmatrix[,i] <- xx
      }
      colnames(cmatrix) <- names
      
      
      # Store matrices for summary metric checks
      cmatrix_all <- cmatrix
      # convert 0 values to minimum positive value to allow log transformation
      cmatrix_filter <- cmatrix
      for (i in 1:length(names)){
        cmin <- min(cmatrix[cmatrix[,i]!=0,i])
        cmatrix_filter[,i] <- ifelse(cmatrix[,i]==0,cmin,cmatrix[,i]) 
      }
      
      ################################################
      # obtain matrix with area-weighted mean explanatory variable for incremental areas between nested monitoring sites
      
      # setup 'df' with incremental site area for use in area-weighting each metric
      waterid <- subdata$waterid
      demiarea <- subdata$demiarea
      idseq <- subdata$staidseq
      df <- data.frame(waterid,demiarea,idseq)
      siteiarea <- sumIncremAttributes(idseq,demiarea,"siteincarea",batch_mode,ErrorOccured)  # sum incremental area by unique siteIDs
      df <- siteiarea
      
      # Code executes 'sumIncremAttributes' function for each explanatory variable:
      # Forest percentage example
      #siteiarea <- sumIncremAttributes(idseq,subdata$forest,"forest_pct")
      #df <- merge(df,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE)
      #df$forest_pct <- df$forest_pct / df$siteincarea * 100
      numsites <- max(subdata$staidseq)
      cmatrixM <- matrix(0,nrow=numsites,ncol=length(names))
      if(numsites>10){
        for (i in 1:length(names)){
          nclass <- paste("subdata$",names[i],sep="")
          xx <- eval(parse(text=nclass)) * demiarea   # area weight variable
          xname <- paste("siteiarea <- sumIncremAttributes(idseq,xx,",shQuote(names[i]),",batch_mode,ErrorOccured)",sep="")
          eval((parse(text=xname)))
          df <- merge(df,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE)
          xname <- paste("cmatrixM[,",i,"] <- df$",names[i]," / df$siteincarea ",sep="")  # compute mean
          eval((parse(text=xname)))
        }
        colnames(cmatrixM) <- names
        
        # Store matrices for summary metric checks
        cmatrixM_all <- cmatrixM
        # convert 0 values to minimum positive value to allow log transformation
        cmatrixM_filter <- cmatrixM
        for (i in 1:length(names)){
          cmin <- min(cmatrixM[cmatrixM[,i]!=0,i])
          cmatrixM_filter[,i] <- ifelse(cmatrixM[,i]==0,cmin,cmatrixM[,i]) 
        }
      }


      ################################################
      # Output Results
      
      filename <- paste(path_results,"/estimate/",file_sum,"_explvars_correlations.pdf",sep="")
      pdf(file=filename,font="Helvetica")
      
      
      #################################################
      # Area-weighted means for the incremental area of monitoring sites 
      
      if(numsites>10){
      par(mfrow=c(1,1))
      strExplanation<-paste("
      Correlation plots for incremental area of monitoring sites includes the following: 
        -Scatter Plot Matrix with Lowess smooths for the raw data
        -Boxplots of the logged raw values of the explanatory variables
      ")
      
      gplots::textplot(strExplanation,valign="top", cex=0.7)
      title("Correlation Results for Explanatory Variables (Site Inc. Areas)")
      
      # plots
      cmatrixM <- na.omit(cmatrixM)                     # omit rows with NA values
      df <- data.frame(cmatrixM)
      colnames(df) <- names
      cor.allValuesM <- cor(df,method=c("spearman"),use="pairwise.complete.obs")
      
      scatterplotMatrix(df,diagonal="boxplot",reg.line=FALSE,use="pairwise.complete.obs",spread=FALSE,smooth=TRUE)
      boxplot(log(df))
      
      }
      
      #################################################
      # Reach-level raw data 
      
      par(mfrow=c(1,1))
      strExplanation<-paste("
      Correlation plots for reach-level data includes the following: 
        -Scatter Plot Matrix with Lowess smooths for the raw data
        -Boxplots of the raw values of the explanatory variables
        -Scatter Plot Matrix with Lowess smooths for the log-transformed data
        -Boxplots of the log-transformed values of the explanatory variables 

         (note that zero values are converted to minimum of non-zero values for the
          log-transformed data)
      ")
      
      gplots::textplot(strExplanation,valign="top", cex=0.7)
      title("Correlation Results for Explanatory Variables (Reaches)")
      
      
      # plots
      cmatrix <- na.omit(cmatrix)                     # omit rows with NA values
      df <- data.frame(cmatrix)
      colnames(df) <- names
      cor.allValues <- cor(df,method=c("spearman"),use="pairwise.complete.obs")
      
      nsamples <- ifelse(rows < maxsamples,rows,maxsamples)
      
      # Untransformed data
      sdf <- sample_n(df,nsamples)
      cor.sampleValues <- cor(sdf,method=c("spearman"),use="pairwise.complete.obs")
      # maximum size limited to 6472 observations
      scatterplotMatrix(sdf,diagonal="boxplot",reg.line=FALSE,use="pairwise.complete.obs",spread=FALSE,smooth=TRUE)
      boxplot(sdf)
      
      # Transformed data
      # convert 0 values to minimum positive value to allow log transformation
      for (i in 1:length(names)){
        cmin <- min(cmatrix[cmatrix[,i]!=0,i])
        cmatrix[,i] <- ifelse(cmatrix[,i]==0,cmin,cmatrix[,i]) 
      }
      cmatrix <- data.frame(log10(cmatrix))        # log transformation
      df <- data.frame(cmatrix)
      colnames(df) <- names
      sdf <- sample_n(df,nsamples)
      cor.sampleLogValues <- cor(sdf,method=c("spearman"),use="pairwise.complete.obs")
      
      # remove variables based on correlations with NAs and resample transformed data
      xnames <- numeric(length(names))
      for (i in 2:length(names)) {
        for (j in 1:(i-1))
        if(is.na(cor.sampleLogValues[i,j])) {
          xnames[i] <- 1 + xnames[i]
          xnames[j] <- 1 + xnames[j]
        }
      }
      cols <- sum(ifelse(xnames<=1,1,0))
      cmatrix <- matrix(0,nrow=rows,ncol=cols)
      nnames <- names[xnames<=1]
      j<-0
      for(i in 1:length(xnames)) {
        if(xnames[i]<=1) {
          j<-j+1
          dname <- paste("subdata$",nnames[j],sep="")
          xx <- eval(parse(text=dname))
          cmatrix[,j] <- xx          
        }
      }
      colnames(cmatrix) <- nnames
      
      cmatrix <- na.omit(cmatrix)                     # omit rows with NA values
      # convert 0 values to minimum positive value to allow log transformation
      for (i in 1:length(nnames)){
        cmin <- min(cmatrix[cmatrix[,i]!=0,i])
        cmatrix[,i] <- ifelse(cmatrix[,i]==0,cmin,cmatrix[,i]) 
      }
      cmatrix <- data.frame(log10(cmatrix))        # log transformation
      df <- data.frame(cmatrix)
      nsamples <- ifelse(rows < maxsamples,rows,maxsamples)
      sdf <- sample_n(df,nsamples)
      # maximum size limited to 6472 observations
      scatterplotMatrix(sdf,diagonal="boxplot",reg.line=FALSE,use="pairwise.complete.obs",spread=FALSE,smooth=TRUE)
      boxplot(sdf)
      
      dev.off()  # shuts down current graphics device
      graphics.off()  # shuts down all open graphics devices
      
      
      # save correlation matrices for output to tables
      Cor.ExplanVars.list <- named.list(nsamples,cor.allValues,cor.sampleValues,cor.sampleLogValues,cor.allValuesM,
                                        cmatrix_all,cmatrix_filter,cmatrixM_all,cmatrixM_filter)

      
      ###################################################
      # Output text file
      # define title output function
      outcharfun<-function(char) {
        outchar <- data.frame(char)
        row.names(outchar ) <- c(" ")
        colnames(outchar ) <- c(" ")
        return(outchar)
      }
      # define "space" for printing
      ch <- character(1)
      space <- data.frame(ch)
      row.names(space) <- ch
      colnames(space) <- c(" ")
      
      
      filename <- paste(path_results,"/estimate/",file_sum,"_explvars_correlations.txt",sep="")
      sink(file=filename,split="FALSE",append=FALSE)
      
      #########################################
      # Monitoring site incremental area results     
      
      if(numsites>10){
      options(width = 200) 
      print(outcharfun("CORRELATION MATRICES FOR EXPLANATORY VARIABLES (Site Incremental Areas)"))
      
      print(outcharfun("SPEARMAN CORRELATIONS FOR ALL OBSERVATIONS"))
      print(cor.allValuesM)
      
      print(space)
      print(outcharfun("SUMMARY METRICS FOR EXPLANATORY VARIABLES (Site Incremental Areas)"))
      print(summary(cmatrixM_all))
      
      print(outcharfun("FILTERED SUMMARY METRICS FOR EXPLANATORY VARIABLES (zero values converted to minimum of non-zero values)"))
      print(summary(cmatrixM_filter))
      }
      
      #########################################
      # Reach-level results
      
      options(width = 200) 
      print(space)
      print(space)
      print(outcharfun("CORRELATION MATRICES FOR EXPLANATORY VARIABLES (Reaches)"))
      
      print(outcharfun("SPEARMAN CORRELATIONS FOR ALL OBSERVATIONS"))
      print(cor.allValues)
      
      xtext <- paste("SPEARMAN CORRELATIONS FOR SUBSAMPLE OF OBSERVATIONS (n=",nsamples,")",sep="")
      print(outcharfun(xtext))
      print(cor.sampleValues)
      
      print(outcharfun("SPEARMAN CORRELATIONS FOR SUBSAMPLED LOGGED OBSERVATIONS (zero values are converted to minimum of non-zero values)"))
      print(cor.sampleLogValues)
      
      
      print(space)
      print(outcharfun("SUMMARY METRICS FOR EXPLANATORY VARIABLES (Reaches)"))
      print(summary(cmatrix_all))
      
      print(outcharfun("FILTERED SUMMARY METRICS FOR EXPLANATORY VARIABLES (zero values converted to minimum of non-zero values)"))
      print(summary(cmatrix_filter))
      
      sink(type="message")
      sink()
      
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("correlationMatrix.R",batch_mode)
      }
    }else{#if no error
 return(Cor.ExplanVars.list)
    }#end if error
    
  }#test if previous error
}#end function

