#'@title replaceNAs
#'@description Replace all NAs with 0's. Output is saved to parent.frame().
#'@param listColumns A named list of variables in which to replace NAs. 
#'Recommend using `named.list()` to generate a named list of variables.
#'@examples
#'x<-c(1,2,3)
#'y<-c(NA,4,5)
#'replaceNAs(named.list(x,y))
#'@keywords external
replaceNAs<-function(listColumns){
columnNames<-names(listColumns)
  for (i in 1:length(columnNames)) { 
    dname <- paste(columnNames[i],"<-ifelse(is.na(",columnNames[i],"),0,",columnNames[i],")",sep="")
    eval(parse(text=dname),envir = parent.frame()) 
  }
}


