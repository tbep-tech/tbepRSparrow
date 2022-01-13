checkDynamic<-function(subdata){
  dynamic<-TRUE
  if (length(names(subdata)[names(subdata) %in% c("year","season")])==0){
    dynamic<-FALSE
  }else if (length(names(subdata)[names(subdata)=="year"])!=0){
    if (all(is.na(subdata$year))){
      if (length(names(subdata)[names(subdata)=="season"])!=0){
        if(all(is.na(subdata$season))){
          dynamic==FALSE
        }
      }else{#no season found
        dynamic<-FALSE
      }
    }
  }else{# no year found
    if (length(names(subdata)[names(subdata)=="season"])!=0){
      if(all(is.na(subdata$season))){
        dynamic==FALSE
      }
    }else{#noseason found
      dynamic<-FALSE
    }
  }
  return(dynamic)
}