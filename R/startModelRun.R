#################################################################################
# 3. Select user-specified model parameters and setup parameters for optimization
#################################################################################
if (exists("runScript")){
  if (runScript=="yes" & ErrorOccured=="no"){
    
message("Reading parameters and design matrix...")
# (A) input parameters and settings
betavalues <- readParameters(path_results,file_vars,csv_decimalSeparator, csv_columnSeparator,if_estimate,
                             if_estimate_simulation,
                             batch_mode,ErrorOccured)

# (B) Setup the parameter and system variables
SelParmValues <- selectParmValues(betavalues,if_estimate,if_estimate_simulation,batch_mode,ErrorOccured)
#print("SELECTED PARAMETER VALUES")
#SelParmValues 
if (ErrorOccured=="no"){
   # deactivates unnecessary computation of parameter correlations, Eigenvalues in cases of one predictor variable
   if(SelParmValues$bcols == 1) {ifHess <- "no"}  
}
# (C) input source-delivery interaction matrix (includes all possible variables)
dmatrixin <- readDesignMatrix(path_results,file_design,betavalues,csv_decimalSeparator, csv_columnSeparator,
                              batch_mode,ErrorOccured)

# (D) Setup design/interaction matrix
dlvdsgn <- selectDesignMatrix(SelParmValues,betavalues,dmatrixin,batch_mode,ErrorOccured)


##############################################################
# 4. Create SUBDATA object for model estimation and prediction                         
##############################################################
if (ErrorOccured=="no"){
message("Creating and Modifying subdata...")}
# (A) Create 'subdata' by filtering DATA1
subdata <- createSubdataSorted(filter_data1_conditions,data1,batch_mode,ErrorOccured)

# (B) DATA MODIFICATIONS FUNCTION allows user-specified modification 
#        of data and parameter variables in SUBDATA
if (if_userModifyData=="yes"){
filesys <- paste(path_results,file_sum,"_userModifyData.R",sep="")
source(filesys)
if (ErrorOccured=="no"){
  tryIt<-try({ 
subdata <- modifySubdata(betavalues,data_names,subdata,class_landuse,lon_limit,batch_mode,
                         path_user,results_directoryName,csv_columnSeparator,csv_decimalSeparator,
                         ErrorOccured)
},TRUE)#end try

if (class(tryIt)=="try-error"){#if an error occured
  ErrorOccured<-"yes"
  cat("\n \n")
  message(paste("AN ERROR OCCURRED IN PROCESSING _userModifyData.R\n",
                geterrmessage(),"RUN EXECUTION TERMINATED.",sep=""))
  if (batch_mode=="yes"){#if batch output message to log
    cat(" \nAN ERROR OCCURRED IN PROCESSING _userModifyData.R\n",
        geterrmessage(),"RUN EXECUTION TERMINATED.",sep="")
  }
  
  if (regexpr("not found",geterrmessage())>0){
    cat("\n \n")
    message(paste("OBJECT NOT FOUND ERROR COULD INDICATE THAT THE VARIABLE DOES NOT EXIST IN THE dataDictionary.csv FILE"))
    if (batch_mode=="yes"){#if batch output message to log
      cat(" \nOBJECT NOT FOUND ERROR COULD INDICATE THAT THE VARIABLE DOES NOT EXIST IN THE dataDictionary.csv FILE\n")
         
    } 
  }
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  exit() 
}else{#if no error
  
}#end if error

  }#test if previous error

}else{
  startEndmodifySubdata(data_names,class_landuse,batch_mode,ErrorOccured)
}
if (ErrorOccured=="no"){
message("Testing for missing variables in subdata...")}
checkClassificationVars(subdata,classvar,class_landuse,batch_mode,ErrorOccured)
checkMissingSubdataVars(subdata,betavalues,path_results,file_sum,batch_mode,ErrorOccured)
###########################################################
# 5. Setup calibration and validation sites and loads
###########################################################

# (A) Calibration site filtering, based on 'calsite' index variable
#     Filter the monitoring sites based on user-specified criteria (above)
#     (Generate site count summary:  numsites1, numsites2, numsites3, numsites4, nMon)
if (ErrorOccured=="no"){
message("Setting up calibration and validation sites...")}
Csites.list <- selectCalibrationSites(subdata,data_names,
                                      minimum_headwater_site_area,
                                      minimum_reaches_separating_sites,
                                      minimum_site_incremental_area,
                                      batch_mode,ErrorOccured)

waterid <- Csites.list$waterid
depvar <- Csites.list$depvar
staid <- Csites.list$staid
staidseq <- Csites.list$staidseq
xx <- data.frame(waterid,staid,staidseq,depvar)
drops <- c("depvar","staid","staidseq")
subdata <- subdata[ , !(names(subdata) %in% drops)]
subdata <- merge(subdata,xx,by="waterid",all.y=FALSE,all.x=FALSE)
subdata <- subdata[with(subdata,order(subdata$hydseq)), ]     # resort by the original HYDSEQ order

numsites1 <- Csites.list$numsites1 
numsites2 <- Csites.list$numsites2
numsites3 <- Csites.list$numsites3
numsites4 <- Csites.list$numsites4
nMon <- Csites.list$nMon
nMoncalsites <- sum(ifelse(subdata$calsites==0 | is.na(subdata$calsites),0,1))

# (B) Select validation sites 
vic <- 0
vsitedata <- NA
Vsites.list <- NA
if(if_validate == "yes") {
  Vsites.list <- selectValidationSites(iseed,pvalidate,subdata,minimum_reaches_separating_sites,data_names,batch_mode,ErrorOccured)

  waterid <- Vsites.list$waterid
  depvar <- Vsites.list$depvar
  staid <- Vsites.list$staid
  staidseq <- Vsites.list$staidseq
  vdepvar <- Vsites.list$vdepvar
  vstaid <- Vsites.list$vstaid
  vstaidseq <- Vsites.list$vstaidseq
  xx <- data.frame(waterid,staid,staidseq,depvar,vstaid,vstaidseq,vdepvar)
  drops <- c("depvar","staid","staidseq")
  subdata <- subdata[ , !(names(subdata) %in% drops)]
  subdata <- merge(subdata,xx,by="waterid",all.y=FALSE,all.x=FALSE)
  subdata <- subdata[with(subdata,order(subdata$hydseq)), ]     # resort by the original HYDSEQ order

  nMon <- Vsites.list$nMon
  nMoncalsites <- sum(ifelse(subdata$calsites==0 | is.na(subdata$calsites),0,1))
  vic <- Vsites.list$vic
  Csites.list$depvar <- Vsites.list$depvar
  Csites.list$staid <- Vsites.list$staid
  Csites.list$nMon <- Vsites.list$nMon
  Csites.list$nMoncalsites <- nMoncalsites
  Csites.list$staidseq <- Vsites.list$staidseq
}
if (ErrorOccured=="no"){
print(paste("Initial monitoring site count: ",numsites1,sep=""))

print(paste("Monitoring sites after filtering for small headwater sites: ",numsites2,sep=""))

print(paste("Monitoring sites after filtering for minimum number of reaches separating sites: ",numsites3,sep=""))

print(paste("Monitoring sites after filtering for minimum incremental area between sites: ",numsites4,sep=""))

print(paste("Number of calibration sites identified by the CALSITES variable: ",nMoncalsites,sep=""))

print(paste("Number of selected calibration sites with non-zero observed loads: ",nMon,sep=""))

print(paste("Number of selected validation sites with non-zero observed loads: ",vic,sep=""))
}#if no error
save(subdata,file=paste(path_results,"/data/subdata",sep=""))

###############################################################
# 6. Missing data checks and data setup for estimation 
###############################################################
if (ErrorOccured=="no"){
message("Setting up data for estimation...")}
# (A) Check for missing data in SUBDATA
checkAnyMissingSubdataVars(subdata,betavalues,batch_mode,ErrorOccured)

# (B) Setup 'Data' and 'beta' matrices and 'data.index.list' for optimization
DataMatrix.list <- createDataMatrix(if_mean_adjust_delivery_vars,subdata,SelParmValues,betavalues,batch_mode,ErrorOccured)

# (B1) Setup 'Data' and 'beta' matrices and 'data.index.list' for optimization w/ scaled parameters
DataMatrixEstimate.list <- createDataMatrixEstimate(if_auto_scaling,if_mean_adjust_delivery_vars,subdata,
                                                    SelParmValues,betavalues,batch_mode,ErrorOccured)


# (C) Setup SITEDATA and VSITEDATA for diagnostics
sitedata <- subdata[(subdata$depvar > 0), ]  # create site attribute object
assign("sitedata",sitedata,envir = .GlobalEnv)
numsites <- length(sitedata$waterid)

# Find minimum and maximum Lat/Lon values for setting mapping projection limits
sitegeolimits <- findMinMaxLatLon(sitedata,lat_limit,lon_limit,mapping.input.list,batch_mode,ErrorOccured)
if (ErrorOccured=="no"){
cat(" \nMonitoring station latitude and longitude minimums and maximums = \n ",sep="")
print(unlist(sitegeolimits))
cat("\n \n")
}#if no error

vnumsites <- 0
if(if_validate == "yes") {
  vsitedata <- subdata[(subdata$vdepvar > 0), ]  # create site attribute object
  assign("vsitedata",vsitedata,envir = .GlobalEnv)
  vnumsites <- length(vsitedata$waterid)
  vnumsites 
}

# (D) Setup land use for incremental basins for diagnostics 
#      (includes setup of 'classvar2' classes in 'sitedata' and 'vsitedata' objects for 
#      plotting diagnostics for non-contiguous variables)
if(numsites > 0) {
  sitedata.landuse <- calcIncremLandUse(subdata,class_landuse,subdata$staidseq,minimum_reaches_separating_sites,batch_mode,ErrorOccured)
  # Obtain total drainage area classification variable for calibration sites
  sitedata.demtarea.class <- calcDemtareaClass(sitedata$demtarea,batch_mode,ErrorOccured)
}
if(vnumsites > 0) {
  vsitedata.landuse <- calcIncremLandUse(subdata,class_landuse,subdata$vstaidseq,minimum_reaches_separating_sites,batch_mode,ErrorOccured)
  # Obtain total drainage area classification variable for validation sites
  vsitedata.demtarea.class <- calcDemtareaClass(vsitedata$demtarea,batch_mode,ErrorOccured)
}

# if no classvar/classrchvar then use sitedata.demtarea.class 5.5.17
if (is.na(classvar) |is.na(classrchvar)){
  sitedata$sitedata.demtarea.class<-sitedata.demtarea.class
  if(vnumsites > 0) {
    vsitedata$sitedata.demtarea.class<-vsitedata.demtarea.class 
  }
  if (is.na(classvar)){
    classvar<-"sitedata.demtarea.class"
  }
  if (is.na(classrchvar)){
    classrchvar<-"sitedata.demtarea.class"
    uniqueClasses<-unique(sitedata.demtarea.class)[order(unique(sitedata.demtarea.class))]
    demtarea.rchclass <- numeric(length(subdata$waterid))
    demtarea.rchclass <- rep(uniqueClasses[1],length(subdata$waterid))
    for (i in 1:length(subdata$waterid)) {
      for (k in 1:(length(uniqueClasses)-1)) {
        if(uniqueClasses[k] < subdata$demtarea[i] & subdata$demtarea[i] <= uniqueClasses[k+1]) {
          demtarea.rchclass[i] <- uniqueClasses[k+1]
        } 
      }
    }
    subdata$sitedata.demtarea.class<-demtarea.rchclass
  }
}


# (E) Run correlations among explanatory variables
#     NOTE: needs conversion to subroutine; HUC8 computation of mean to reduce size needs generalization
Cor.ExplanVars.list <- NA
if (select_corr == "yes") {
  maxsamples <- 500
  # select user-specified names from 'parmCorrGroup' in the parameters.csv file
  names <- SelParmValues$sparrowNames[SelParmValues$bCorrGroup==1]
  ntype <- SelParmValues$betatype[SelParmValues$bCorrGroup==1]
  if(length(names)>1) {   # minimum of 2 explanatory variables required
    message("Running correlations among explanatory variables...")
    Cor.ExplanVars.list <- correlationMatrix(path_results,file_sum,maxsamples,names,ntype,SelParmValues,subdata,batch_mode,ErrorOccured)
  }
}

########################################
### 7. PERFORM MODEL EXECUTION TASKS ###
########################################

# variables from prior data processing subroutines: numsites,data_names,sitedata.landuse,vsitedata.landuse,
#        Csites.weights.list

# Calculate regression weights as proportional to incremental area size
weight<-NA
Csites.weights.list <- named.list(weight)
if(numsites > 0) {
  Csites.weights.list <- setNLLSWeights(NLLS_weights,subdata,sitedata,data_names,minimum_reaches_separating_sites,batch_mode,ErrorOccured)
  if(NLLS_weights == "default") {
    Csites.weights.list$weight <- rep(1,numsites) # set weights=1.0 (overide area-based weights)
  }
}
assign("Csites.weights.list",Csites.weights.list,envir = .GlobalEnv)

runTimes <- controlFileTasksModel(
  # pathnames
  path_results,file_sum,
  # parameters
  SelParmValues,
  # NLR weights
  Csites.weights.list,
  # data
  subdata,DataMatrixEstimates.list,DataMatrix.list,sitedata,Vsites.list,vsitedata,
  # land use classifcation 
  classvar,classrchvar,class_landuse,
  # explanatory variable correlations
  Cor.ExplanVars.list,
  # estimation
  data_names,minimum_reaches_separating_sites,
  if_estimate,if_predict,dlvdsgn,if_validate,
  estimate.input.list,reach_decay_specification,reservoir_decay_specification,
  # diagnostics
  sitedata.landuse,vsitedata.landuse,sitedata.demtarea.class,vsitedata.demtarea.class,
  # mapping 
  mapping.input.list,
  # bootstrapping
  iseed,biters,
  #scenarios
  scenario_name,
  #batch_mode
  batch_mode,
  #error trap
  ErrorOccured,
  if_estimate_simulation,class_landuse_percent,
  #for read-write csv files
  csv_decimalSeparator, csv_columnSeparator
)  

if (ErrorOccured=="no"){
# print Hessian SE calculation Run time 
#if(ifHess=="yes") {
#  cat(" \nHessian run time\n ")
#  print(runTimes$HesRunTime)
#} 

if(if_boot_estimate == "yes") {
  cat(" \nBootstrap estimation run time\n ")
  print(runTimes$BootEstRunTime)
}

if(if_boot_predict == "yes") {
  cat(" \nBootstrap prediction run time\n ")
  print(runTimes$BootPredictRunTime)
}

if(!is.na(master_map_list[1])) {
  cat(" \nMap predictions run time\n ")
  print(runTimes$MapPredictRunTime)
}
}#if no error

#output user settings
allSettings<-outputSettings(path_results,file_sum,csv_decimalSeparator, csv_columnSeparator,TRUE,
                            batch_mode,ErrorOccured)

#run model comparison
modelCompare(path_results,file_sum,compare_models,modelComparison_name,if_auto_scaling,if_diagnostics,
             csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)

#popup results
try({
  if ((if_estimate=="yes" | if_estimate_simulation=="yes") & ErrorOccured=="no"){
  shell.exec(paste(path_results,"estimate/",run_id,"_diagnostic_plots.pdf",sep=""))
  shell.exec(paste(path_results,"estimate/",run_id,"_summary.txt",sep=""))
  }
},silent=TRUE)

if (enable_interactiveMaps=="yes" & batch_mode=="no" & ErrorOccured=="no"){
#setup for interactive Mapping

# obtain uncertainties, if available
objfile <- paste(path_results,"/predict/",file_sum,"_BootUncertainties",sep="")
if(file.exists(objfile) == TRUE) {
  load(objfile)
  map_uncertainties <- c("se_pload_total","ci_pload_total")
} else {
  map_uncertainties <- NA 
  BootUncertainties <- NA
}





shiny::runApp(shinyMap2(#stream/catchment
  path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  #predict.list,
  subdata,SelParmValues,
  #site attr
  sitedata,
  #scenarios
  scenario_name,estimate.list,
  ConcFactor,DataMatrix.list,
  reach_decay_specification,reservoir_decay_specification,
  #scenarios out
  add_vars,csv_decimalSeparator,csv_columnSeparator,
  #batchError
  batch_mode,ErrorOccured))
stopApp()


}#end interactive maps

  }#if runScript=yes
}#if exists(runScript)