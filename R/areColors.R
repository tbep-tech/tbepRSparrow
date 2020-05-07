#'@title areColors
#'@description Tests color strings for validity 
#'@param x single color string or vector of color strings
#'@return logical TRUE/FALSE indicating whether or not invalid color strings were found

areColors <- function(x) {
  #https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
  testCol<- sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
  #end sourced from https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
  testCol<-unique(testCol)
  if (length(testCol)==1){
      if (suppressWarnings(testCol==TRUE)){
    testCol<-TRUE
      }else{
        testCol<-FALSE
      }
  }else{
    testCol<-FALSE
  }
  return(testCol)
}