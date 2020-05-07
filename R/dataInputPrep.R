#'@title dataInputPrep
#'@description Import data1 and dataDictionary
#'Check for duplicate sparrowNames
#'Replace data1UserNames with sparrowNames in data1 object
#'Check navigation variables for large integers and all missing values
#'Create and/or verify navigation variables and demtarea
#'Save data1 and dataDictionary as _priorImport
#'Save data_names and data1 to .GlobalEnv
#'Uses subroutines: readData, readVarnames, checkDupVarnames, replaceData1Names, checkData1NavigationVars, 
#'createVerifyReachAttr, checkMissingData1Vars, errorOccurred.
#'@param path_data path to users data directory
#'@param input_data_fileName name of users data1 file
#'@param csv_decimalSeparator decimal separator for csv output
#'@param csv_columnSeparator column separator for csv output
#'@param path_results path to results directory
#'@param file_sum user specified run_id 
#'@param if_reverse_hydseq yes/no indicating whether hydseq in the DATA1 file needs to be reversed from sparrow_control
#'@param if_verify_demtarea specify whether or not to verify demtarea
#'@param calculate_reach_attribute_list list of attributes to calculate
#'@param path_gis path to gis directory
#'@param mapping.input.list mapping.input.list from control file
#'@param batch_mode yes/no indicating whether or not batch processing is used
#'@param ErrorOccured yes/no indicating if a previous error has occured `ErrorOccured<-"no"`

dataInputPrep<-function(#for readData
                        path_data,input_data_fileName,csv_decimalSeparator, csv_columnSeparator,
                        #for readVarnames
                        path_results,file_sum,
                        #for checkData1NavigationVars
                        if_reverse_hydseq,
                        #for createVerifyNavigationVars
                        if_verify_demtarea,calculate_reach_attribute_list,
                        path_gis,mapping.input.list,
                        #for all
                        batch_mode,ErrorOccured){

  if (ErrorOccured=="no"){
    tryIt<-try({ 

######################################
# 2. DATA1 input and data preparation
######################################      

        message("Running Data import and prep...")

# (A) Input DATA1 CSV file
data1<-readData(path_data,input_data_fileName,csv_decimalSeparator, csv_columnSeparator,
                batch_mode,ErrorOccured)


# (B) input VARNAMES (data dictionary) CSV file
#     (should contain all required network variables plus any new variables
#      needed for mapping, plus any variables in SUBDATA used to make calculations)
data_names <- readVarnames(path_results,file_sum,csv_decimalSeparator, csv_columnSeparator,
                           batch_mode,ErrorOccured)

# (B1) check varNames for duplicates
checkDupVarnames(data_names,batch_mode,ErrorOccured)

# (C) Replace the column names in DATA1 with the required column names
#    Set default setting for 'calsites' index if missing or all NAs
data1 <- replaceData1Names(data_names,data1,batch_mode,ErrorOccured)

# (C1) Checks for excessively large integer values for navigation variables and 
#      replacement of values if necessary
data1 <- checkData1NavigationVars(data1,if_reverse_hydseq,batch_mode,ErrorOccured)


# (D) Optional processing of NETWORK ATTRIBUTES AND VERIFY DRAINAGE AREA
data1 <-  createVerifyReachAttr(if_verify_demtarea,calculate_reach_attribute_list,data1,
                                         path_results,file_sum,path_gis,mapping.input.list,
                                         csv_decimalSeparator, csv_columnSeparator,
                                         batch_mode,ErrorOccured)
  


# (E) Check for missing required variables in DATA1 as specified in varnames file
data1 <- checkMissingData1Vars(data1,batch_mode,ErrorOccured)


#save dataInputPrep objects to binary file for future use
fileList<-c("data1","data_names")
fileList<-fileList[which(fileList %in% ls())]
fileName<-strsplit(path_results,"/")[[1]]
fileName<-paste(fileName[1:length(fileName)-1],collapse = "/")
fileName<-paste(fileName,"/",gsub(".csv","",input_data_fileName),"_priorImport",sep="")
save(list=fileList, file=fileName)


  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("dataInputPrep.R",batch_mode)
      }
    }else{#if no error
      assign("data1",data1,envir = .GlobalEnv)
      assign("data_names",data_names,envir = .GlobalEnv)
    }#end if error
    
  }#test if previous error
  
  
}#end function