#'@title createInitialDataDictionary
#'@description Function to creates dataDictionary.csv file based on the column names of the data1 file,
#'Adds missing required and fixed variables with data1UserName=="NA".
#'Opens the new dataDictionary.csv file for the user to edit.
#'Terminates execution of RSPARROW.
#'Uses subroutines: readData, getVarList, errorOccurred.
#'@param path_data path to users data directory
#'@param input_data_fileName name of users data1 file
#'@param file_sum user specified run_id 
#'@param path_results path to results directory
#'@param create_initial_parameterControlFiles yes/no character string indicating whether or not parameter.csv and design_matrix.csv files are to be created.
#'@param csv_decimalSeparator decimal separator for csv output
#'@param csv_columnSeparator column separator for csv output
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`


createInitialDataDictionary<-function(path_data,input_data_fileName,file_sum,path_results,
                                create_initial_parameterControlFiles,csv_decimalSeparator, csv_columnSeparator,
                                batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({

  if (file.exists(file.path(paste(dirname(path_results),"/dataDictionary.csv",sep="")))==FALSE){
  #read data1 or indata
data1<-readData(path_data,input_data_fileName,csv_decimalSeparator, csv_columnSeparator,batch_mode,ErrorOccured)

#create varnames file
initialVarnames<-as.data.frame(matrix(rep(NA,length(data1)),ncol=4,nrow=length(data1)))
names(initialVarnames)<-c("varType","sparrowNames","data1UserNames","varunits")
initialVarnames$data1UserNames<-names(data1)

#match any required or fixed varnames
fixed<-as.character(getVarList()$fixNames)
required<-as.character(getVarList()$reqNames)
initialVarnames$sparrowNames = ifelse(tolower(initialVarnames$data1UserNames) %in% c(fixed,required),tolower(initialVarnames$data1UserNames),NA)
initialVarnames$varType = ifelse(tolower(initialVarnames$data1UserNames) %in% fixed,"FIXED",
                                 ifelse(tolower(initialVarnames$data1UserNames) %in% required,"REQUIRED",NA))

#get explanations for fixed and required variables
initialVarnames<-merge(initialVarnames,getVarList()$explanation,by="sparrowNames",all.x=TRUE)

#reorder
initialVarnames<-initialVarnames[match(names(data1),initialVarnames$data1UserNames),match(c("varType","sparrowNames","data1UserNames","varunits","explanation"),names(initialVarnames))]

missingRequired<-required[which(!required %in% initialVarnames$sparrowNames)]
if (length(missingRequired)!=0){
message("MISSING REQUIRED sparrowNames :")
for (i in missingRequired){
     message(i)
}
}#if missing required
cat("\n \n")
missingFixed<-fixed[which(!fixed %in% initialVarnames$sparrowNames)]
if (length(missingFixed)!=0){
message("MISSING FIXED sparrowNames : ")
for (i in missingFixed){
  message(i)
}
}#if missing fixed

#add missing names to top of initialvarnames
missing<-data.frame(sparrowNames = c(missingRequired,missingFixed))
if (nrow(missing)!=0){
 missing<-merge(missing,getVarList()$explanation, by="sparrowNames")
 missing$varType<-ifelse(missing$sparrowNames %in% fixed,"FIXED","REQUIRED")
 missing$data1UserNames<-rep(NA,nrow(missing))
 missing$varunits<-missing$data1UserNames
 missing<-missing[,match(c("varType","sparrowNames","data1UserNames","varunits","explanation"),names(missing))]
 initialVarnames<-rbind(missing,initialVarnames)
}

#write varnames
fwrite(file=paste(dirname(path_results),"/dataDictionary.csv",sep=""),initialVarnames,
       row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec=csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
cat("\n \n")
message(paste("INITIAL dataDictionary FILE : ",paste(dirname(path_results),"/dataDictionary.csv",sep="")," AVAILABLE FOR EDIT",sep=""))
shell.exec(paste(dirname(path_results),"/dataDictionary.csv",sep=""))

if (create_initial_parameterControlFiles=="no"){
cat("\n \n")
message("RUN EXECUTION TERMINATED")
}
}else{#varnames already exists
  message(paste(paste(dirname(path_results),"/",file_sum,"_dataDictionary.csv",sep="")," ALREADY EXISTS.\n
NEW dataDictionary FILE NOT CREATED.\n
SET create_initial_dataDictionary<-'no' to RUN RSPARROW WITH CURRENT dataDictionary.",sep=""))
  if (create_initial_parameterControlFiles=="no"){
    cat("\n \n")
    message("RUN EXECUTION TERMINATED")
  }
}
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("createInitialDataDictionary.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function
  