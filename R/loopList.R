#name1 is for output to list or reactive values object
#name2 is object used for name1
#onlyNames TRUE for output names only 
#start/end for bounds of loop
#strInputName name of txt or select box
loopList<-function(name1, name2, strInputName, start, end, onlyNames){
  goodNames<-character(0)
  for (i in start:end){
    if (onlyNames==FALSE){
    eval(parse(text = paste0("name1$",strInputName,i,"<-name2$",strInputName,i)))
    }else{
      goodNames<-c(goodNames,paste0(strInputName,i))
    }
  }
  
  if (onlyNames==TRUE){
    return(goodNames)
  }else{
    return(name1)
  }
}