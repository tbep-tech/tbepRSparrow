interactiveFactorHelp<-function(input){
  if (input$domain=="all reaches"){
  showModal(modalDialog(
    title = "Source adjustment reduction (or increase) factors",
    
    renderUI({
      
      if (length(input$sourcesCheck)!=0){
        strSource<-paste(input$sourcesCheck,collapse = ", ")
        if (input$factors=="" | length(eval(parse(text=input$factors)))!=length(input$sourcesCheck)){
          strSeq<-paste(seq(0.1,1.25,length.out = length(input$sourcesCheck)),collapse = ", ")
          reductIncrease<-ifelse(seq(0.1,1.25,length.out = length(input$sourcesCheck))<1,"reduction","increase")
          str5<-paste(paste(paste("<b>",input$sourcesCheck,"</b>",sep="")," has ",reductIncrease," factor of ",seq(0.1,1.25,length.out = length(input$sourcesCheck)),sep=""),collapse="<br/>")
        }else{
          strSeq<-paste(eval(parse(text=input$factors)),collapse = ", ")
          reductIncrease<-ifelse(eval(parse(text=input$factors))<1,"reduction","increase")
          str5<-paste(paste(paste("<b>",input$sourcesCheck,"</b>",sep="")," has ",reductIncrease," factor of ",eval(parse(text=input$factors)),sep=""),collapse="<br/>")
        }
      }else{
        strSource<-paste("Source1","Source2","Source3",collapse=", ")
        strSeq<-paste(seq(0.1,1.25,length.out = 3),collapse = ", ")
        reductIncrease<-ifelse(seq(0.1,1.25,length.out = 3)<1,"reduction","increase")
        str5<-paste(paste(c("<b>Source1</b>","<b>Source2</b>","<b>Source3</b>")," has ",reductIncrease," factor of ",seq(0.1,1.25,length.out = 3),sep=""),collapse="<br/>")
        
      }  
      
      str1<-"Source adjustment reduction (or increase) factors apply to 'all reaches'"
      str2<-"for example, enter 0.1 or 1.1 for a 10% reduction or increase in sources, respectively"
      str3<-paste("Enter factors to align with selected sources: <b>'c(",strSource,")'</b> from left to right",sep="")
      str4<-paste("<b>Example :</b>"," c(",strSeq , ")",sep="")
      HTML(paste(str1,str2," <br/>",str3," <br/>",str4,str5,sep="<br/>"))
      
      
    })
    
  ))
  }else{#select reaches
    showModal(modalDialog(
      title = "Reach Selection function(s)",
      
      renderUI({
        
        if (length(input$sourcesCheck)!=0){
          strSource<-paste(input$sourcesCheck,collapse = ", ")
          lengthFactors<-substr(input$selectReaches,3,nchar(input$selectReaches)-1)
          lengthFactors<-length(trimws(strsplit(lengthFactors,";")[[1]],"both"))
          if (input$selectReaches=="" | lengthFactors!=length(input$sourcesCheck)){
            strSeq<-seq(0.1,1.25,length.out = length(input$sourcesCheck))
            strSeq<-paste("ifelse(huc2==5,",strSeq,",1) ",collapse="; ")
            str5<-paste(paste(paste("<b>",input$sourcesCheck,"</b>",sep="")," has reach selection function of ",strsplit(strSeq,";")[[1]],sep=""),collapse="<br/>")
          }else{
            strSeq<-input$selectReaches
            str5<-paste(paste(paste("<b>",input$sourcesCheck,"</b>",sep="")," has reach selection function of ",strsplit(strSeq,";")[[1]],sep=""),collapse="<br/>")
          }
        }else{
          strSource<-paste("Source1","Source2","Source3",collapse=", ")
          strSeq<-seq(0.1,1.25,length.out = 3)
          strSeq<-paste("ifelse(huc2==5,",strSeq,",1) ",collapse="; ")
          str5<-paste(paste(c("<b>Source1</b>","<b>Source2</b>","<b>Source3</b>")," has reach selection function of ",strsplit(strSeq,";")[[1]],sep=""),collapse="<br/>")
        }  
        
        str1<-"Reach Selection function(s) apply to 'select reaches'"
        str2<-"for example, enter ifelse(huc2==5,0.1,1) or ifelse(huc2==5,1.1,1) for a 10% reduction or increase in sources in reaches flagged with huc2==5, respectively"
        str3<-paste("Enter functions to align with selected sources: <b>'c(",strSource,")'</b> from left to right. 
                    Functions MUST be separated by a semi-colon",sep="")
        str4<-paste("<b>Example :</b>"," c(",strSeq , ")",sep="")
        HTML(paste(str1,str2," <br/>",str3," <br/>",str4,str5,sep="<br/>"))
        
        
      })
      
    ))
  }
}#end function