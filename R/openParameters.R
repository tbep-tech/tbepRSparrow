openParameters<-function(path_user,results_directoryName){
  #test filePath
  if (!file.exists(paste(path_user,"/",results_directoryName,"/parameters.csv",sep=""))){
    message(cat("NO PARAMETERS FILE FOUND IN RESULTS DIRECTORY.\n",paste(path_user,"/",results_directoryName,"/",sep="")))
  }else{
  shell.exec(paste(path_user,"/",results_directoryName,"/parameters.csv",sep=""))}
}