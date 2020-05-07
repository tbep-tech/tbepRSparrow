#'@title mapBreaks
#'@description Function to create mapping color breakpoints
#'Uses subroutines: errorOccurred.
#'@param vvar mapping variable as vector
#'@param colors user specified  character string of mapping color breakpoints
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch mode
#'@param ErrorOccured yes/no indicating if a previous error has occured.  Function is only run if `ErrorOccured=="no"`
#'@return vector of breakpoints

mapBreaks<-function(vvar,  colors,batch_mode,ErrorOccured){

# link MAPCOLORS for variable to shape object (https://gist.github.com/mbacou/5880859)
# Color classification of variable
iprob<-length(colors)
set_unique_breaks <- function(x,ip) {
  chk1 <- quantile(x, probs=0:ip/ip)
  chk <- unique(quantile(x, probs=0:ip/ip)) # define quartiles
  # exit if the condition is met
  if (length(chk1) == length(chk)) return(ip)
  ip<-ip-1
  Recall(x,ip) # run the function again
}
iprob <- set_unique_breaks(vvar,iprob)


   brks <- quantile(vvar, probs=0:iprob/iprob)




  
  return(named.list(brks,iprob))
 
}