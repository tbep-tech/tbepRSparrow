#
# eigensort.R
#
##############################################################################
# function to sort eigenvectors and eigenvalues in decreasing order
#  Input: 'eigs' object containing eigenvalues and vectors from R 'eigen' function
#  Output: 'eigout' object with eigenvalues and vectors sorted in decreasing order
eigensort <- function(eigs,batch_mode,ErrorOccured) {
  if (ErrorOccured=="no"){
    tryIt<-try({ 

  temp <- sort.int(eigs$values, index.return=TRUE, decreasing=TRUE)
  values <- temp$x
  n <- nrow(eigs$vectors)
  vectors <- matrix(0,nrow=n,ncol=n)
  for (i in 1:ncol(eigs$vectors)) {
    vectors[,i] <- eigs$vectors[,temp$ix[i]]
  }
  eigenout <- list("values"=values,"vectors"=vectors)
 
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("eigensort.R",batch_mode)
      }
    }else{#if no error
       return(eigenout)
    }#end if error
    
  }#test if previous error
}#end function
