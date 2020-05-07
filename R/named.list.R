#
# named.list.R
#
###########################################################
# function to create named list in object
named.list <- function(...) { 
  l <- setNames( list(...) , as.character( match.call()[-1]) ) 
  l
}
