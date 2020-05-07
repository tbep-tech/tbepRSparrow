openDesign<-function(path_user,results_directoryName){
  #test filePath
  if (!file.exists(paste(path_user,"/",results_directoryName,"/design_matrix.csv",sep=""))){
    message(cat("NO DESIGN MATRIX FILE FOUND IN RESULTS DIRECTORY.\n",paste(path_user,"/",results_directoryName,"/",sep="")))
  }else{
    shell.exec(paste(path_user,"/",results_directoryName,"/design_matrix.csv",sep=""))}
}