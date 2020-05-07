#
# selectDesignMatrix.R
#
####################################################
# Create design matrix for subset of parameters

selectDesignMatrix <- function(SelParmValues,betavalues,dmatrixin,batch_mode,ErrorOccured){
  if (ErrorOccured=="no"){
    tryIt<-try({ 
 
# prep for design matrix
  pselect <- SelParmValues$pselect
  srcselect <- ifelse(betavalues$parmType == "SOURCE" & pselect == 1,1,0)
  dlvselect <- ifelse(betavalues$parmType == "DELIVF" & pselect == 1,1,0)
  
  asrc <- sum(ifelse(betavalues$parmType == "SOURCE",1,0))   # total number of source variables
  adel <- sum(ifelse(betavalues$parmType == "DELIVF",1,0))   # total number of delivery variables
  ndeliv <- sum(dlvselect)
  nsrc <- sum(srcselect)
  sdmatrix <- logical(length=asrc)
  dmatrix <- logical(length=adel)
  sdmatrix <- ifelse(pselect[betavalues$parmType == "SOURCE"] == 1,TRUE,FALSE)
  dmatrix <- ifelse(pselect[betavalues$parmType == "DELIVF"] == 1,TRUE,FALSE)

  d2matrixin <- matrix(unlist(as.data.frame(dmatrixin)), ncol=adel, nrow=asrc)
  dlvdsgnO <- numeric(adel*asrc)
  k <- 0
  for (i in 1:asrc) {
    for (j in 1:adel) {
      k <- k+1
      dlvdsgnO[k] <- d2matrixin[i,j]
    }
  }

# obtain matrix consistent with variable selections if delivery variables selected
# Reduce size of dlvdsgnO to be consistent with variable selections

  dlvdsgn1 <- matrix(unlist(dlvdsgnO))
  if (ndeliv > 0) {
   dlvdsgn <- matrix(1, ncol=ndeliv, nrow=nsrc)
   isrc = 0
   jdel = 0
   jtot = 0
   for (i in 1:asrc){
    if(sdmatrix[i]==TRUE){
      isrc=isrc+1
      for (j in 1:adel){
        jtot=jtot+1
        if(dmatrix[j]==TRUE){
          jdel=jdel+1
          dlvdsgn[isrc,jdel] = dlvdsgn1[jtot]
        }
      }
      jdel=0
    } else {jtot=jtot+adel}
   }
  } else {
    dlvdsgn <- dlvdsgn1
  }

    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("selectDesignMatrix.R",batch_mode)
      }
    }else{#if no error
  return(dlvdsgn)
    }#end if error
    
  }#test if previous error
}#end function



