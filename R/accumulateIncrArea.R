#'@title errorOccurred
#'@description AFunction to accumulate incremental area.  Function can only be run afer replaceData1Names.R.  Uses hydrologic sequencing data.
#'Uses subroutines: errorOccurred. 
#'Used By subroutines : calcAccumIncrArea
#'@param indata input data (data1 or subdata)
#'@param accum_elements character vector of columns to be accumulated (case sensitive)
#'@param accum_names character vector of final names for accumlated data (case sensitive)
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return AreaRch data.frame containing waterid and accum_elements 


accumulateIncrArea<-function(indata,accum_elements,accum_names,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  #get data
  #data<-get(as.character(bquote(indata)))
  data<-indata
      
  #replace any NAs with 0
  for (c in c("termflag","fnode","tnode",accum_elements)){
    eval(parse(text = paste("data$",c,"<-ifelse(is.na(data$",c,"),0,data$",c,")",sep="")))
  }
  
  data <- data[(data$fnode > 0 & data$tnode > 0 & data$termflag != 3), ]
  
  #order by hydrologic sequence
  data<-data[order(data$hydseq),]
  
  #get relevant variables
  maxArc<-max(c(data$fnode,data$tnode))
  nr<-length(data$waterid)
  iup<-data$fnode
  idown<-data$tnode
  frac<-data$frac
  reach<-data$waterid
  
  #loop through accum_elements
  for (e in accum_elements){
  area<-eval(parse(text = paste("data$",e,sep="")))
  
  carea<-rep(0,maxArc)
  arearch<-rep(0,nr)
  
  for (i in 1:nr){
    carea[idown[i]]<-carea[idown[i]] + (frac[i] * carea[iup[i]] + area[i])
    arearch[i] <- frac[i] * carea[iup[i]] + area[i]
  }
  
if (e==accum_elements[1]){
  AreaRch<-data.frame(temp = arearch)
  names(AreaRch)<-accum_names[which(accum_elements==e)]
}else{
  AreaRch<-as.data.frame(cbind(AreaRch,data.frame(temp = arearch)))
  names(AreaRch)[length(AreaRch)]<-accum_names[which(accum_elements==e)]                       
}

  }
  
  #add waterid for merging
  AreaRch$waterid<-reach
  

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("accumulateIncrArea.R",batch_mode)
      }
    }else{#if no error
      return(AreaRch)
    }#end if error
    
  }#test if previous error
  
  
}#end function