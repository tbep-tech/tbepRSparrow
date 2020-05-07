# setNLLSWeights.R
#
# Set NLLS regression weights based on incremental area size
#
####################################################################
# Assign site IDs to incremental areas by climbing network structure
#  Notes:
#   -reaches sorted by HYDSEQ
#   -input: 'staid' - reach site IDs (non-zero for reaches with selected monitoring sites)
#   -returns: 'rchstaid' - site IDs assigned to upstream incremental reaches
# 
###################################################################
 setNLLSWeights <- function(NLLS_weights,subdata,sitedata,data_names,minimum_reaches_separating_sites,batch_mode,ErrorOccured) {
   if (ErrorOccured=="no"){
     tryIt<-try({

#############################################
# create global variables for calculations

 datalstreq <- data_names$sparrowNames
 for (i in 1:length(datalstreq)) {
  dname <- paste("subdata$",datalstreq[i],sep="")
  x1name <- paste(datalstreq[i],sep="")
  if((x1name %in% names(subdata)) == TRUE) {
    assign(x1name,eval(parse(text=dname)))
  }
 }
############################################
 minnum <- minimum_reaches_separating_sites
 staidseq <- assignIncremSiteIDs(minnum,staid,waterid,tnode,fnode,batch_mode,ErrorOccured)   # call function to assign sites to reaches
 xx <- data.frame(staidseq,demiarea)
 count<-ddply(xx,.(staidseq), summarize, nirchs=length(staidseq))      # get count for unique staids
 count <- count[-1,]  # delete first row
 siteiarea<-ddply(xx,.(staidseq),summarize,tiareakm2=sum(demiarea))    # sum incr areas for unique staids
 siteiarea <- siteiarea[-1,]  # delete first row

# Update SITEDATA, merge COUNT and SITEIAREA by staidseq  
 xx <- sitedata
 xx <- merge(xx,count,by="staidseq",all.y=FALSE,all.x=TRUE)  
 xx <- merge(xx,siteiarea,by="staidseq",all.y=FALSE,all.x=TRUE) 
 xx <- xx[with(xx,order(xx$hydseq)), ]   # resort dataframe by the original HYDSEQ order
 tiareakm2 <- xx$tiareakm2
 
 # select user-specified weight
 if(NLLS_weights=="lnload" | NLLS_weights=="user") {
   weight <- sitedata$weight     # estimated in 'userModifyData' function 'runWeightedErrors'
 } else {    # NLLS_weights=="area"
   if(NLLS_weights=="area") {
     weight <- log(xx$tiareakm2) / mean(log(xx$tiareakm2))
     weight <- 1.0 / weight**2    # Incremental area-based weight; large values receive less weight
     weight <- weight * mean(1/weight)     # Normalized weights
   } else {
     weight<-NA  # default setting where monitoring sites exist
   }
 }    
    
 # Check for NA weights
 if(NLLS_weights != "default") {
   if(sum(ifelse(is.na(weight),1,0))>0 | is.null(weight)) {
     assign("ErrorOccured","yes",envir = .GlobalEnv)
     assign("ErrorOccured","yes",envir = parent.frame())
     Csites.weights.list <- named.list(NLLS_weights,tiareakm2,count,weight)
     assign("Csites.weights.list",Csites.weights.list,envir = .GlobalEnv)
     cat("\n \n")
     message("ERROR: Missing values found for calibration sites in variable 'weight'. \n User requested weighted estimation (see 'NLLS_weights' control setting). \n For the NLLS_weights=='lnload' option, the variable 'weight' must be computed in userModifyData and \n entered into the dataDictionary.csv as sparrowNames with FIXED varType. \nRUN EXECUTION TERMINATED.")
     if (batch_mode=="yes"){#if batch output message to log
       cat(" \nERROR: Missing values found for calibration sites in variable 'weight'. \n User requested weighted estimation (see 'NLLS_weights' control setting) \n For the NLLS_weights=='lnload' option, the variable 'weight' must be computed in userModifyData and \n entered into the dataDictionary.csv as sparrowNames with FIXED varType. \nRUN EXECUTION TERMINATED.")
     }
     exit <- function() {
       .Internal(.invokeRestart(list(NULL, NULL), NULL))
     }
     exit() 
   }
 }
 
 Csites.weights.list <- named.list(NLLS_weights,tiareakm2,count,weight)

     },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("set_NLLS_weights.R",batch_mode)
       }
     }else{#if no error
 return(Csites.weights.list)
     }#end if error
     
   }#test if previous error
 }#end function
