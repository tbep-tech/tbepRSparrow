#sets removed boxes to ""
removeLoopList<-function(inputs, goodNames){
  for (n in names(inputs)[which(!names(inputs) %in% goodNames)]){
    eval(parse(text = paste0("inputs$",n,"<-''")))
  }
  return(inputs)
}