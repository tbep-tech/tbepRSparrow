installPackages <- function(if_install_packages,path_master){
  
  if(substr(path_master,nchar(path_master),nchar(path_master))=="/"){
    path_master<-substr(path_master,1,nchar(path_master)-1)
    assign("path_master",path_master,.GlobalEnv)
  }
  
  if (if_install_packages=="yes"){
    
    pkgs<-c(    "devtools", 
                "numDeriv", 
                "nlmrt",    
                "stringr", 
                "gplots", 
                "ggplot2",
                "plyr",
                "OpenMx",
                "rgdal",                
                "maptools",
                "sp",
                "spdep",   
                "data.table",
                "rstudioapi",
                "roxygen2",
                "svGUI",
                "svDialogs",
                "shiny",
                "shinyWidgets",
                "rhandsontable",
                "gear", 
                "car",
                "dplyr", 
                "sf", 
                "mapview", 
                "leaflet"
                )
    assign("pkgs",pkgs,envir = .GlobalEnv)
    for (p in pkgs){
      assign("p",p,envir = .GlobalEnv)
  if (!require(p,character.only = TRUE,quietly=TRUE)){
    try({
      install.packages(p, repos="http://ftp.ussg.iu.edu/CRAN/",quiet=TRUE)
      detachAllPackages()
      },TRUE)
    if(!require(p,character.only = TRUE,quietly=TRUE)) stop("Package not found")
      }
    }

  }#if_install_pacakges
}#end function