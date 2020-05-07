
endModifySubdata<-function(betavalues,data_names,subdata,class_landuse, batch_mode,ErrorOccured,env = parent.frame()){
#####################################################################################
  if (ErrorOccured=="no"){
    tryIt<-try({  
 
  #check for missing landuse class
missingLanduseClass<-class_landuse[which(!class_landuse %in% data_names$sparrowNames)]
if (length(na.omit(missingLanduseClass))!=0){
  for (i in 1:length(missingLanduseClass)){
    cat("\n FATAL ERROR : MISSING class_landuse : ",missingLanduseClass[i],"\n ",sep="")
    cat("\n \n")
  }
}

# substitute 0.0 for NAs for user-selected parameters
# set NAs for explanatory variables associated with the selected parameters
eval(parse(text=paste("replaceNAs(named.list(",paste(paste("'",betavalues$sparrowNames[betavalues$parmMax != 0],"'",sep=""),collapse=","),"))",sep="")),envir = parent.frame())

# Transfer global variables to SUBDATA

# Refresh variables in 'subdata' (this allows subsequent use of subdata values)
#  (accounts for any modification to these variables to replace NAs or
#   following calculations in the data modifications section)
datalstreq <- data_names$sparrowNames
for (i in 1:length(datalstreq)) {
  dname <- paste("subdata$",datalstreq[i]," <- ",datalstreq[i],sep="") 
  eval(parse(text=dname),envir = parent.frame()) 
}

# Ensure that variables associated with user-selected parameters are reassigned to SUBDATA 
for (i in 1:length(betavalues$sparrowNames)) {
  if(betavalues$parmMax[i]>0){
    dname <- paste("subdata$",betavalues$sparrowNames[i]," <- ",betavalues$sparrowNames[i],sep="") 
    eval(parse(text=dname),envir = parent.frame()) 
  }
}



    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("endModifySubdata.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function