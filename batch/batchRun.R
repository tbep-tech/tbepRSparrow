#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
load(gsub("batchRun.R","batch.RData",res))
if (exists("runScript")){
  remove(list=c("runScript","runRsparrow"))

devtools::load_all(path_main,recompile = FALSE)

runScript<-"yes"
runRsparrow<-"yes"
sink(file=paste(path_results,"/batchSessionInfo/",run_id,"_log.txt",sep=""),split=TRUE)
cat("\n \n")
cat("RSPARROW MODEL NAME: ",file_sum,sep="")
cat("\n \n")
if (if_predict_scenarios=="yes"){
  cat("SCENARIO NAME: ",scenario_name,sep="")
  cat("\n \n")
}
cat("OUTPUT DIRECTORY: ",path_results,sep="")
cat("\n \n")

  # Section 2. DATA1 input and data preparation

if (load_previousDataImport=="yes"){
  fileName<-strsplit(path_results,"/")[[1]]
  fileName<-paste(fileName[1:length(fileName)-1],collapse = "/")
  fileName<-paste(fileName,"/",gsub(".csv","",input_data_fileName),"_priorImport",sep="")
  #check if file exists
  if (file.exists(fileName)){
    load(file=fileName)  
  }else{
    ErrorOccured<-"yes"
    cat("ERROR : ",fileName," NOT FOUND\n SET load_previousDataImport<-'no'.\n RUN EXECUTION TERMINATED.",sep="")
  }
  
  
}else{
  dataInputPrep(#for readData
    path_data,input_data_fileName,csv_decimalSeparator, csv_columnSeparator,
    #for readVarnames
    path_results,file_sum,
    #for checkData1NavigationVars
    if_reverse_hydseq,
    #for createVerifyNavigationVars
    if_verify_demtarea,calculate_reach_attribute_list,
    path_gis,mapping.input.list,
    #for all
    batch_mode,ErrorOccured)
}

###############################################################
#runRsparrow
source(paste(path_main,"/R/startModelRun.R",sep=""))

save.image(file=paste(path_results,"/batchSessionInfo/",run_id,".RData",sep=""))
sink()
}

}