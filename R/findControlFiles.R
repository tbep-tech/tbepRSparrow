#test for all control files existance terminate if not there
findControlFiles<-function(path_user,if_userModifyData,
                           create_initial_dataDictionary, create_initial_parameterControlFiles){
      
      exit <- function() {
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }
      
   path_results <- paste(path_user,"/",results_directoryName,sep="") # location of the results directory
 
   
  #save control files
  filesList<-c("sparrow_control.R",
               "parameters.csv",
               "design_matrix.csv",
               "userModifyData.R",
               "dataDictionary.csv")
  if (if_userModifyData=="no"){
    filesList<-filesList[which(filesList!="userModifyData.R")]
  }
  if (create_initial_dataDictionary=="yes"){
    filesList<-filesList[which(!filesList %in% c("dataDictionary.csv","design_matrix.csv","parameters.csv"))]
  }
  if (create_initial_parameterControlFiles=="yes"){
    filesList<-filesList[which(!filesList %in% c("design_matrix.csv","parameters.csv"))]
  }
  fileExist<-sapply(filesList, function(x) file.exists(paste(path_results,"/",x,sep="")))
                   
  fileExist<-data.frame(success = t(fileExist)[1,])
  fileExist$path<-paste(path_results,"/",filesList,sep="")
  fileExist<-fileExist[which(fileExist$success==FALSE),]
  
  if (nrow(fileExist)!=0){
    for (x in fileExist$path){
      message(paste("MISSING CONTROL FILES.\n \n",x,"\n \nRUN EXECUTION TERMINATED",sep=""))
    }
    assign("ErrorOccured","yes",envir = .GlobalEnv)
    assign("ErrorOccured","yes",envir = parent.frame())
exit()
  }
  

}