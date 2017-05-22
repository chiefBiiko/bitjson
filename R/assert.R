# bitjson assertions

#' 
isRData <- function(x) {
  if (is.object(x) | is.vector(x) | is.atomic(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' 
isTruthyChar <- function(string) {
  if (is.character(string) && nchar(string) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' 
isValidRemoteName <- function(remotename) {
  if (is.character(remotename) && 
      grepl('http(s)?://.+\\..+', remotename, perl=TRUE)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}