makePaths<-function(path_user, path_master,run_id,results_directoryName,data_directoryName,gis_directoryName){
  path_data <- paste(path_user,"/",data_directoryName,"/",sep="")       # location of the DATA1 file
  path_results <- paste(path_user,"/",results_directoryName,"/",run_id,"/",sep="") # location of the results directory
  path_gis <- paste(path_user,"/",gis_directoryName,sep="")          # GIS shape files (necessary to exclude the slash at end)
  
  #test if data, results, and gis are all at same level
  badPath<-NA
  for (p in c("path_data","path_results","path_gis")){
    path<-get(p)
    if (p=="path_results"){
      path<-paste(path_user,"/",results_directoryName,"/",sep="")
    }
    if (!dir.exists(path)){
      badPath<-c(badPath,p)
    }
  }
  if (length(na.omit(badPath))!=0){
    ErrorOccured<-"yes"
    assign("ErrorOccured","yes",envir = .GlobalEnv)
    message(paste("ERROR : THE FOLLOWING REQUIRED PATHS ARE NOT FOUND IN THE USER DIRECTORY\n ",
                  paste(" \n",na.omit(badPath)," : ",get(na.omit(badPath)),"\n",sep=""),"\nPATHS TO DATA, GIS, AND RESULTS MUST ALL EXIST IN THE USER DIRECTORY : \n ",
                  path_user," \nTHE CONTROL FILE SHOULD BE RUN FROM THE UPPER LEVEL OF THE RESULTS DIRECTORY SHOWN HERE : \n ",
                  paste(path_user,"/",results_directoryName,"/",sep=""),"\nRUN EXECUTION TERMINATED",sep=""))
  }
  path_main<-path_master
  path_src <- paste(path_main,"/src/",sep="")
  path_master <- paste(path_main,"/R/",sep="")
  file_sum <- run_id                                  # name for output files
  file_vars <- run_id                                 # name of input parameter CSV file
  file_design <- run_id                               # design matrix CSV file
  file_varnames <- run_id                             # varnames (data dictionary) CSV file
  for (n in ls()){
    assign(n,get(n),envir=.GlobalEnv)
  }
}