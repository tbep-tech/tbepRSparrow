
# current script file (in full path)
# works with Rscript, source(),Rgui.exe, or in RStudio Run selection

findScriptName <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    path<-normalizePath(sub(needle, "", cmdArgs[match]))
    #return(path)
  } else {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      path<-normalizePath(sys.frames()[[1]]$fileName) 
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        path<-normalizePath(sys.frames()[[1]]$ofile)
      } else {
        if (class(try({path<-normalizePath(rstudioapi::getActiveDocumentContext()$path)},TRUE))=="try-error"){
          message("Please select current control.R file.  Browser window may appear behind Rstudio.")
          path<-file.choose()
        }else        
          # RStudio Run Selection
        # http://stackoverflow.com/a/35842176/2292993 
       path<-normalizePath(rstudioapi::getActiveDocumentContext()$path)
      }
    }
  }

  assign("path_user",dirname(dirname(path)),envir = .GlobalEnv)
  return(path)
}