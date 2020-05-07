# 
# sumIncremAttributes.R
#
# Function sums specified attribute over the incremental area of the selected calibration sites
# Merges the new attribute with the calibration site dataframe 'sitedata'
#
####################################################################
# Assign site IDs to incremental areas by climbing network structure
#  Notes:
#   -reaches sorted by HYDSEQ
#   -input: 'staid' - reach site IDs (non-zero for reaches with selected monitoring sites)
#   -returns: 'rchstaid' - site IDs assigned to upstream incremental reaches
# 
# Input:  attrib - specified attributes with length equal to number of reaches
#         attrib_name - the character name of the attribute
# Output 'siteiarea' dataframe with summed attribute
####################################################################

sumIncremAttributes <- function(idseq,attrib,attrib_name,batch_mode,ErrorOccured) { 
  if (ErrorOccured=="no"){
    tryIt<-try({
   
  xx <- data.frame(idseq,attrib)
  count<-ddply(xx,.(idseq), summarize, nirchs=length(idseq))      # get count for unique staids
  count <- count[-1,]  # delete first row
 # 928 sites with only one reach count
  siteiarea<-ddply(xx,.(idseq),summarize,tiareakm2=sum(attrib))    # sum attribute for unique staids
  siteiarea <- siteiarea[-1,]  # delete first row with "0" idseq
  colnames(siteiarea) <- c("idseq",attrib_name)
  
 # Update SITEDATA with the summed attribute
#  sitedata <- merge(sitedata,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE) 
  
  
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("sumIncremAttributes.R",batch_mode)
      }
    }else{#if no error
      return(siteiarea)
    }#end if error
    
  }#test if previous error
}#end function

############### EOF
