#'@title copyPriorModelFiles
#'@description Function to copy a previously run model control files into the users results directory.  
#'RSPARROW is terminated after files are copied and sparrow_control is opened in Rstudio.
#'Uses subroutines: errorOccurred.
#'@param activeFile path to current sparrow_control file
#'@param old_run_id character string indicating the previously run model control files to copy into the results directory
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`



copyPriorModelFiles<-function(activeFile,old_run_id, path_master,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  closed<-menu(c("Yes","No"),title=cat("You have selected to run copy_PriorModelFiles <-'",old_run_id,"'\n \nPlease close all active control files now.  Including \n",activeFile," \nSelect 'Yes' after all files are closed.\n \nTo cancel copy_PriorModelFiles select 'No'.",sep=""))
 if (closed==1){
   if (basename(activeFile)!="sparrow_control.R"){
     message("copy_PriorModelFiles MUST be run from the sparrow_control.R file in the results directory.\n copy_PriorModelFiles FAILED.\n  RUN EXECUTION TERMINATED.")
   
     }else{#copy files
     #get path_results
       path_results<-dirname(activeFile)
       path_old<-paste(path_results,"/",old_run_id,"/",sep="")
       path_oldFile<-paste0(path_old,old_run_id,"_sparrow_control.R")
       
       #replace path_master in old file with activeFile path_master
       #read control file as text
       x <- readLines(path_oldFile)
       #find where path_master is designated
       editthis<-x[which(regexpr("path_master<-",gsub(" ","",x))>0 &  regexpr("#path_master",x)<0)]
       #replace with current path_master
       y <- gsub( editthis, paste0("path_master<-'",path_master,"'"), x )
       #overwrite the file
       cat(y, file=path_oldFile, sep="\n")
       
       #list necessary files
       filesList<-c("sparrow_control.R",
                    "parameters.csv",
                    "design_matrix.csv",
                    "userModifyData.R",
                    "dataDictionary.csv")
       filesListFrom<-paste(old_run_id,"_",filesList,sep="")
       
       #copy files
       for (f in 1:length(filesList)){
         if (file.exists(paste(path_old,filesListFrom[f],sep=""))){
         file.copy(from = paste(path_old,filesListFrom[f],sep=""),
                   to=paste(path_results,"/",filesList[f],sep=""),
                   overwrite = TRUE)
         }else{
           ErrorOccured<-"yes"
           message("MISSING control file \n",paste(path_old,filesListFrom[f],sep=""),"\ncopy_PriorModelFiles FAILED.\n  RUN EXECUTION TERMINATED. ")
         }
       }
       
       if (ErrorOccured=="no"){
         file.edit(paste(path_results,"/sparrow_control.R",sep=""))
         message(paste("copy_PriorModelFiles COMPLETE.  Control files ready for edit in \n",path_results,sep=""))
       }
       
   }
 
   }else{
   message("Files NOT closed.  copy_PriorModelFiles FAILED.\n  RUN EXECUTION TERMINATED.")
 }#make sure closed
 
 #assign("runOld","yes",.GlobalEnv)

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      # assign("runOld","yes",.GlobalEnv)
      if(ErrorOccured=="no"){
        errorOccurred("copyPriorModelFiles.R",batch_mode)
      }
     
    }else{#if no error

    }#end if error
    
  }#test if previous error
}#end function