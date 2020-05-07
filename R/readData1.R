#
# readData1.R
#
###########################################################
# INPUT DATA1 FILE
###########################################################
#  NOTE:  add a sequence number in a first column of DATA1

readData1 <- function(path,fileType){
  if (fileType=="csv"){
 filedata1 <- paste(path,"data1.csv",sep="")        
 data1 <- read.csv(filedata1,header=TRUE,stringsAsFactors=FALSE)
  }else{
    data1<-get(load(paste(path,"data1",sep="") ))
  }
 return(data1)
}


