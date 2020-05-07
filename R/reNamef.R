#rename active control files
reNamef<-function(oldName,newName,create_initial_varnames,create_initial_parameterControlFiles,
                  if_userModifyData,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

   #get current path
   path<-str_split(oldName,"[\\\\]|[^[:print:]]")[[1]]
  #get name of current script
   oldName<-path
   oldName<-gsub(".R","",oldName[length(oldName)])
   #remove script from path
   path<-paste(path[1:(length(path)-1)],collapse="/")
  if (create_initial_varnames=="no" & create_initial_parameterControlFiles=="no"){   
   #test if newName already exists ask if user wants to overwrite
    filesList<-c(
                 "_parameters.csv",
                 "_design_matrix.csv",
                 "_userModifyData.R",
                 "_dataDictionary.csv",
                 ".R")
   if (if_userModifyData=="no"){
     filesList<-filesList[which(filesList!="_userModifyData.R")]
   }
   existNew<-0
   for (f in filesList){
     cfile<-paste(path,"/",newName,f,sep="")
     if (file.exists(cfile)){
       existNew<-existNew+1
     }
   }
   
   #test if old control files exist
   existOld<-NA
   for (f in filesList){
     cfile<-paste(path,"/",oldName,f,sep="")
     if (!file.exists(cfile)){
       existOld<-c(existOld,cfile)
     }
   }
   if (length(na.omit(existOld))==0){
     continue<-"yes"
   }else{
     continue<-"no"
   #stop if old files not found
     cat("The following files were not found : \n\n")
     cat(paste(na.omit(existOld),collapse=", \n"))
     cat("\n\nRun execution terminated.")
     out<-continue
   return(out)
   } 
   stopifnot(continue=="yes")
   
   #rename files in core directory
   if (!exists("continue")){
     continue<-"yes"
   }
   if (existNew==0 | continue=="yes"){
     for (f in filesList){
       if (file.rename(paste(path,"/",oldName,f,sep=""), 
                                                 paste(dirname(paste(path,"/",newName,f,sep="")),"/",newName,f,sep=""))==FALSE & newName!=oldName){
         message(paste("CURRENT ",f," FILENAME DOES NOT MATCH run_id\nPLEASE CLOSE THE FILE BEFORE RUNNING RSPARROW",sep=""))
         continue<-"no"
         return(continue)
         stopifnot(continue=="yes")
       }
     }
       #sapply(filesList, function(x) )
   }
   stopifnot(continue=="yes")
file.edit(paste(dirname(paste(path,"/",newName,".R",sep="")),"/",newName,".R",sep=""))
   out<-continue
   

  
  }else{#if create_initial_varnames=="yes" | create_initial_parameterControlFiles=="yes"
    file.rename(paste(path,"/",oldName,".R",sep=""), 
                paste(dirname(paste(path,"/",newName,".R",sep="")),"/",newName,".R",sep=""))
    file.edit(paste(dirname(paste(path,"/",newName,".R",sep="")),"/",newName,".R",sep=""))
    out<-"no"
}
  
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("reNamef.R",batch_mode)
      }
    }else{#if no error
  return(out) 
    }#end if error
    
  }#test if previous error
}#end function

