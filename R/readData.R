#'@title readData
#'@description This reads the users input data (either .csv or binary format)
#'@param path this is users path_data automatically found
#'@param input_data_fileName this is the users fileName for the input data, if csv must include the file extension
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured `ErrorOccured<-"no"`
#'
readData <- function(path,input_data_fileName,csv_decimalSeparator, csv_columnSeparator,
                     batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({  

      ptm <- proc.time()
      
         print(path)
         print(input_data_fileName)
         filedata1 <- paste(path,input_data_fileName,sep="")  
         print(regexpr(".csv",input_data_fileName))
  if (regexpr(".csv",input_data_fileName)>0){
    data1 <- read.csv(filedata1,header=TRUE,stringsAsFactors=FALSE,
                      dec = csv_decimalSeparator,sep=csv_columnSeparator)
  
    data1BinaryName<-gsub(".csv","",input_data_fileName)
    
    save(data1, file=paste(path_data,data1BinaryName,sep=""))
  }else{
    load(filedata1)
  }
 
  print("head(data1)\n\n")
   cat("head(data1)\n\n")
   print(head(data1))
   cat("\n\n")
   cat("nrow(data1)\n\n")
   print(nrow(data1))
   cat("\n\n")
   cat("Time elapsed during data import.\n ")
   print(proc.time() - ptm)
   cat("\n\n")
   
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("readData.R",batch_mode)
      }
    }else{#if no error
      return(data1)
    }#end if error
    
  }#test if previous error
}#end function

