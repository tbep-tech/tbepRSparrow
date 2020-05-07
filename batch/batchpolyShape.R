#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("batchpolyShape.R","batch.RData",res))
  
  library(rgdal)
  library(sp)

  polyShape <- readOGR(dsn=path_gis, layer=polyShapeName)
  polyShape <- spTransform(polyShape, CRS(CRStext))
  
  objfile <- paste(path_gis,"/polyShape",sep="")
  save(polyShape,file=objfile)
  
  }