sourceRedFunc<-function(shinyInput){
  if (shinyInput$domain=="all reaches"){
    out<-list(scenario_all_factors = as.numeric(as.character(shinyInput$`nsSourceRedALL-hot`$PercentReduction))/100,
              scenario_sources = as.character(shinyInput$`nsSourceRedALL-hot`$Source))
    
  }else if (shinyInput$allSrc=="yes"){
    selectFuncs<-character(0)
   #get all sources and selections
     srcs<-shinyInput$`nsSourceRed-hot`

     sels<-shinyInput$`nsAllSources-hot`
     #remove empty rows
    srcs<-srcs[apply(srcs,1, function(x) any(!is.na(x))),]

    #loop through srcs
    for (s in 1:nrow(srcs)){
      selectFunc<-paste0("S_",as.character(srcs$Source)[s],"<-")
      
      #start ifFunc
      ifFunc<-paste0("ifelse(")
      #loop through selection
      totCrit<-character(0)

      for (i in 1:nrow(sels)){
        var<-as.character(sels$SelectionVariable[i])
        mn<-as.numeric(as.character(sels$Min[i]))
        mx<-as.numeric(as.character(sels$Max[i]))
        #equals can be numeric or character
        eq<-ifelse(suppressWarnings(!is.na(as.numeric(as.character(sels$Equals[i])))),
                   as.numeric(as.character(sels$Equals[i])),
                   as.character(sels$Equals[i]))
        lk<-as.character(sels$Like[i])
        seps<-ifelse(as.character(sels$Separator[i])!="",ifelse(as.character(sels$Separator[i])=="OR","|","&"),"")

        criteria<-ifelse(!is.na(eq) & class(eq)=="character",paste0(var,"=='",eq,"'"),#equals character
                         ifelse(!is.na(eq) & class(eq)!="character",paste0(var,"==",eq),#equals numeric
                                ifelse(!is.na(lk),paste0("regexpr('",lk,"',",var,")>0"),#like
                         ifelse(!is.na(mn)& is.na(mx),paste0(var,">=",mn),#min only
                                ifelse(!is.na(mx) & is.na(mn),paste0(var,"<=",mx),#max only
                                       paste0("(",mn,"<=",var," & ",var,"<=", mx,")")))))) #min and max
        

        if (nrow(sels)==1){
          totCrit<-criteria
        }else if (i!=nrow(sels)){
          totCrit<-paste0(totCrit,criteria," ",seps," ")
        }else{
          totCrit<-paste0(totCrit,criteria)
        }

        #build ifelse
        if (i==nrow(sels)){#end function
          ifFunc<-paste0(ifFunc,totCrit,",",as.numeric(as.character(srcs$PercentReduction[s]))/100,",1)")
        }
      }
      selectFunc<-paste0(selectFunc,ifFunc)
      selectFuncs<-c(selectFuncs,selectFunc)
    }

    out<-list(scenario_sources = as.character(srcs$Source),
              selectFuncs = selectFuncs)
  }else{#complex select reaches
    selectFuncs<-character(0)
    selects<-shinyInput$`nsAllSourcesNO-hot`
    selects<-selects[apply(selects,1, function(x) any(!is.na(x))),]
      for (s in unique(selects$Source)){
       
        src<-selects[which(selects$Source==as.character(s)),]
      
        selectFunc<-paste0("S_",as.character(s),"<-")
        ifFuncs<-character(0)
        
        for (r in unique(src$PercentReduction)){
          #start ifFunc
           
          ifFunc<-paste0("ifelse(")
          #loop through selection
          totCrit<-character(0)
          reds<-src[which(src$PercentReduction==as.character(r)),]
          for (i in 1:nrow(reds)){#for each selection variable
          var<-as.character(reds$SelectionVariable[i])
          mn<-as.numeric(as.character(reds$Min[i]))
          mx<-as.numeric(as.character(reds$Max[i]))
          #equals can be numeric or character
          eq<-ifelse(suppressWarnings(!is.na(as.numeric(as.character(reds$Equals[i])))),
                     as.numeric(as.character(reds$Equals[i])),
                     as.character(reds$Equals[i]))
          lk<-as.character(reds$Like[i])
          seps<-ifelse(as.character(reds$Separator[i])!="",ifelse(as.character(reds$Separator[i])=="OR","|","&"),"")
          
          criteria<-ifelse(!is.na(eq) & class(eq)=="character",paste0(var,"=='",eq,"'"),#equals character
                           ifelse(!is.na(eq) & class(eq)!="character",paste0(var,"==",eq),#equals numeric
                                  ifelse(!is.na(lk),paste0("regexpr('",lk,"',",var,")>0"),#like
                                         ifelse(!is.na(mn)& is.na(mx),paste0(var,">=",mn),#min only
                                                ifelse(!is.na(mx) & is.na(mn),paste0(var,"<=",mx),#max only
                                                       paste0("(",mn,"<=",var," & ",var,"<=", mx,")")))))) #min and max
          
          if (nrow(reds)==1){
            totCrit<-criteria
          }else if (i!=nrow(reds)){
            totCrit<-paste0(totCrit,criteria," ",seps," ")
          }else{
            totCrit<-paste0(totCrit,criteria)
          }
   
          #build ifelse
          if (i==nrow(reds)){#end function
            ifFunc<-paste0(ifFunc,totCrit,",",as.numeric(as.character(r))/100)
          }
          
          }
          if(length(unique(src$PercentReduction))==1){
          ifFuncs<-paste0(ifFuncs,ifFunc)
                          #,",1)")
       # }else if (r!=unique(src$PercentReduction)[length(unique(src$PercentReduction))]){
      #    ifFuncs<-paste0(ifFuncs,",",ifFunc,",1",rep(")",nrow(src)))
        }else if (r!=unique(src$PercentReduction)[1]){
          ifFuncs<-paste0(ifFuncs,",",ifFunc)
        }else{
          ifFuncs<-ifFunc
        }
        }

        selectFunc<-paste0(selectFunc,ifFuncs,",1",paste0(rep(")",length(unique(src$PercentReduction))),collapse = ""))
        selectFuncs<-c(selectFuncs,selectFunc)
  }

    out<-list(scenario_sources = as.character(unique(selects$Source)),
              selectFuncs = selectFuncs)
  }

  out<-c(shinyInput,out)
  
  
  return(out)
}