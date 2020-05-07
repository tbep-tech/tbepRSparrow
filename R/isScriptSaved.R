isScriptSaved<-function(scriptName,testDir){

  if (dir.exists(testDir)){
    msgText<-cat("Did you save the active control file \n",scriptName," \nand all '*.csv' control files?\n \nAre ALL results files closed?\n \nPrevious model subdirectory exists with current run_id\nAll files in the estimate, predicts, maps, and scenarios directories will be deleted if option '1' is selected below\n \nTo overwrite results files ALL results files MUST be closed including _summary.txt and _diagnostic_plots.pdf.\n \n",sep="")
  }else{      
    msgText<-cat("Did you save the active control file \n",scriptName," \nand all '*.csv' control files?\n \n",sep="")
  }
  saved<-menu(c("Yes, I have saved all control files. Continue the current run.",
                "No, I haven't saved all control files.  Cancel current run."),title=msgText)
  return(saved)
}