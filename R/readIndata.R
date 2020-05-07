#
# readIndata.R
#
###########################################################
# INPUT SAS INDATA FILE
###########################################################
#  NOTE:  add a sequence number in a first column of INDATA file

readIndata <- function(path){
  if (fileType=="csv"){
 filedata1 <- paste(path,"indata.csv",sep="")        
 data1 <- read.csv(filedata1,header=TRUE,stringsAsFactors=FALSE)
  }else{
   data1<-get(load(paste(path,"indata",sep="")))
  }
 return(data1)
}


