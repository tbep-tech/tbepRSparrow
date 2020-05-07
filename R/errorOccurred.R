#'@title errorOccurred
#'@description Terminates program
#'@param scriptName name of script where error occurred

errorOccurred<-function(scriptName,batch_mode){
 ErrorOccured<-"yes"
cat("\n \n")
message(paste("AN ERROR OCCURRED IN PROCESSING ", scriptName, "\n",
              geterrmessage(),"RUN EXECUTION TERMINATED.",sep=""))
if (batch_mode=="yes"){#if batch output message to log
  cat(" \nAN ERROR OCCURRED IN PROCESSING ", scriptName, "\n",
      geterrmessage(),"RUN EXECUTION TERMINATED.",sep="")
}
assign("ErrorOccured","yes",envir = .GlobalEnv)
assign("ErrorOccured","yes",envir = parent.frame())
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
exit() 
}
