#'@title removeObjects
#'@description Remove objects from the global environment if the object exists.  
#'Warnings will be suppressed.
#'@param remove.list a character vector of objects to be removed
#'@examples
#'x<-c(1,2,3)
#'y<-data.frame(column1 = c(NA,4,5))
#'z<-"string"
#'removeObjects(c("x","y","z"))
#'@keywords internal

removeObjects<-function(remove.list){
   suppressWarnings(rm( list = Filter( exists, remove.list),envir = .GlobalEnv ))
}

