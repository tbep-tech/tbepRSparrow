#add to inputs list or reactive values
addList<-function(name1,name2,subnames, goodNames, onlyNames){
  for (n in subnames){
    if (onlyNames==FALSE){
    eval(parse(text = paste0("name1$",n,"<-name2$",n)))
    }else{
      goodNames<-c(goodNames,n)
      
    }
  }
  if (onlyNames==FALSE){
    return(name1) 
  }else{
    return(goodNames)
  }
 
}