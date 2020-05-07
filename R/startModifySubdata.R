startModifySubdata<-function(betavalues,data_names,subdata,batch_mode,ErrorOccured,env = parent.frame()){
##############################################################
# Transfer required variables (data_names) to global variables for calculations

### CREATE GLOBALS ONLY WITHIN THE DATA_MODS FUNCTION for both (1) required variables (datalstreq)
# [NOTE:required variables should include any newly defined variables, such as land use or HUC, if these
# will be used as diagnostic contiguous or non-contiguous classification variables; need to be added
# to the VARNAMES CSV file if used]; (2) any variables in SUBDATA used to make calculations;
# and (3) the user-selected parameters...

# ADD create_global_variables.R functionality (create required variables) to modifications
# assign required reach network system variables as global variables
#  (some variables may have NAs and require further calculations in the data modifications section)
  if (ErrorOccured=="no"){
    tryIt<-try({

datalstreq <- data_names$sparrowNames
for (i in 1:length(datalstreq)) {
  dname <- paste("subdata$",datalstreq[i],sep="")
  x1name <- paste(datalstreq[i],sep="")
  if((x1name %in% names(subdata)) == TRUE) {
    assign(x1name,eval(parse(text=dname)),env = parent.frame())
  }
}

# Assign parameter variables to global variables in function
#  Checks for existence and transfers variables from 'subdata'

for (i in 1:length(betavalues$sparrowNames)) {
  dname <- paste("subdata$",betavalues$sparrowNames[i],sep="") 
  x1name <- paste(betavalues$sparrowNames[i],sep="")
  if((x1name %in% names(subdata)) == TRUE) {
    assign(betavalues$sparrowNames[i],eval(parse(text=dname)),env = parent.frame())
  }
}



    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      if(ErrorOccured=="no"){
        errorOccurred("startModifySubdata.R",batch_mode)
      }
    }else{#if no error
      
    }#end if error
    
  }#test if previous error
}#end function