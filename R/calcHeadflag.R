#'@title calcHeadflag
#'@description calculates headflag (translated from SAS hydseq code)
#'Uses subroutines: errorOccurred. 
#'@param data1 dataset to use for headflag calculation
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return data.frame with waterid and headflag


calcHeadflag<-function(data1, batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({
 
  # transfer required variables to global environment from data1
  datalstCheck <- as.character(getVarList()$varList)
  for (i in 1:length(datalstCheck)) {
    dname <- paste("data1$",datalstCheck[i],sep="") 
    x1name <- paste(datalstCheck[i],sep="")
    if((x1name %in% names(data1)) == TRUE) {
      assign(datalstCheck[i],eval(parse(text=dname)))
    }
  }
  
  #create sequence variable
  SEQ<-data.frame(seqvar = seq(1,nrow(data1),1))
  #add seqvar to tnode and fnode
  tnode<-as.data.frame(cbind(SEQ,tnode))
  fnode<-as.data.frame(cbind(SEQ,fnode))
  
  #sort data
  tnode<-tnode[order(tnode$tnode),]
  fnode<-fnode[order(fnode$fnode),]
  
  #save rownumbers
  fnode$Row<-seq(1,nrow(fnode),1)
  tnode$Row<-seq(1,nrow(tnode),1)
  
  fnode<-data.frame(fnode = unique(fnode$fnode))
  
  #save as headwaterflag in data1
  ifhead<-na.omit(fnode[which(!tnode$tnode %in% fnode$fnode),])
    data1$headflag<-ifelse(data1$fnode %in% ifhead,1,0)
  
    outdata<-data1[,which(names(data1) %in% c("waterid","headflag"))]
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("calcHeadflag.R",batch_mode)
      }
    }else{#if no error
      return(outdata)
    }#end if error
    
  }#test if previous error
  
  
}#end function