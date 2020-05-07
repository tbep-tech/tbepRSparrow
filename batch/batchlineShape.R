#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("batchlineShape.R","batch.RData",res))
  
  library(rgdal)
  library(sp)
 
  lineShape <- readOGR(dsn=path_gis, layer=lineShapeName)
  lineShape <- spTransform(lineShape, CRS(CRStext))
  
  objfile <- paste(path_gis,"/lineShape",sep="")
  save(lineShape,file=objfile)
   
}