#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("batchGeoLines.R","batch.RData",res))
  
  library(rgdal)
  library(sp)
  
  #load and convert shape file
  LineGeo <- readOGR(dsn=path_gis, layer=LineShapeGeo)
  LineGeo <- spTransform(LineGeo, CRS(CRStext))
  GeoLines <- as(LineGeo, "SpatialLinesDataFrame")
  
  #save file
  objfile <- paste(path_gis,"/GeoLines",sep="")
  save(GeoLines,file=objfile) 
}