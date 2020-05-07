#
# setupMaps.R
#
#########################
# A preprocessor to read shape files and store objects

 setupMaps <- function(path_gis,mapping.input.list,batch_mode,ErrorOccured) {
   if (ErrorOccured=="no"){
     tryIt<-try({

  # Setup variable lists 
  # create global variable from list names (mapping.input.list)
  for(i in 1:length(mapping.input.list)){
    tempobj=mapping.input.list[[i]]
    eval(parse(text=paste(names(mapping.input.list)[[i]],"= tempobj")))
  }
    if (length(na.omit(convertShapeToBinary.list))!=0){
      #message("Creating binary maps...")
      save(list = c(as.character(outputSettings(path_results,file_sum,csv_decimalSeparator,csv_columnSeparator,FALSE,batch_mode,ErrorOccured)$setting),
                    ls()[which(regexpr("path_",ls())>0)],"path_gis",
                    ls()[which(regexpr("file_",ls())>0)],"mapping.input.list"),
           file=paste(path_main,"/batch/batch.RData",sep=""))
      
      # Setup GEOLINES 
  if("LineShapeGeo" %in% convertShapeToBinary.list & !is.na(LineShapeGeo)) {
    if (file.exists(paste(path_gis,"/GeoLines",sep=""))){
      message("WARNING: PREVIOUSLY GENERATED GeoLines BINARY FILE FOUND.
'LineShapeGeo' FROM converShapeToBinary.list NOT USED
TO GENERATE A NEW GeoLines FILE THE EXISTING FILE MUST BE DELETED FROM THE GIS DIRECTORY\n")
    }else{
      message("Creating GeoLines binary file...")
      system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,"/batch/batchGeoLines.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
    }
  } 
  
  # STREAMS
  # Read shape files and reproject as lat-lon
   if("lineShapeName" %in% convertShapeToBinary.list & !is.na(lineShapeName)) {
     if (file.exists(paste(path_gis,"/lineShape",sep=""))){
       message("WARNING: PREVIOUSLY GENERATED lineShape BINARY FILE FOUND.
'lineShapeName' FROM converShapeToBinary.list NOT USED
TO GENERATE A NEW lineShape FILE THE EXISTING FILE MUST BE DELETED FROM THE GIS DIRECTORY\n")
     }else{
       message("Creating lineShape binary file...")
       system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,"/batch/batchlineShape.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
       
   }
}
  # CATCHMENTS
  # Read shape files and reproject as lat-lon
   if("polyShapeName" %in% convertShapeToBinary.list) {
     if (file.exists(paste(path_gis,"/polyShape",sep=""))){
       message("WARNING: PREVIOUSLY GENERATED polyShape BINARY FILE FOUND.
'polyShapeName' FROM converShapeToBinary.list NOT USED
TO GENERATE A NEW polyShape FILE THE EXISTING FILE MUST BE DELETED FROM THE GIS DIRECTORY\n")
     }else{
       message("Creating polyShape binary file...")
       system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,"/batch/batchpolyShape.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
       
   }
   }  


 
       if (output_map_type=="both" & !is.na(master_map_list)[1]){
         if (!file.exists(paste(path_gis,"/lineShape",sep=""))){
           message("WARNING: lineShape BINARY FILE CREATED FROM lineShapeName IS NOT FOUND. \nSTREAM MAPS WILL NOT BE GENERATED\n ")
         }
         if (!file.exists(paste(path_gis,"/polyShape",sep=""))){
           message("WARNING: polyShape BINARY FILE CREATED FROM polyShapeName IS NOT FOUND. \nCATCHMENT MAPS WILL NOT BE GENERATED\n ")
         }
       }else if (output_map_type=="stream" & !file.exists(paste(path_gis,"/lineShape",sep="")) & !is.na(master_map_list)[1]){
           message("WARNING: lineShape BINARY FILE CREATED FROM lineShapeName IS NOT FOUND. \nSTREAM MAPS WILL NOT BE GENERATED\n ")
       }else if (output_map_type=="catchment" & !file.exists(paste(path_gis,"/polyShape",sep="")) & !is.na(master_map_list)[1]){
           message("WARNING: polyShape BINARY FILE CREATED FROM polyShapeName IS NOT FOUND. \nCATCHMENT MAPS WILL NOT BE GENERATED\n ")
       }
   
      }#if convertShapeToBinary.list !=NA    
     
       },TRUE)#end try
     
     if (class(tryIt)=="try-error"){#if an error occured
       if(ErrorOccured=="no"){
         errorOccurred("setupMaps.R",batch_mode)
       }
     }else{#if no error

     }#end if error
     
   }#test if previous error
 }#end function
