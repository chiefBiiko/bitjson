# bitjson

#' @internal
serializeDataToBits <- function(x) {
  return(as.integer(rawToBits(serialize(x, connection=NULL))))
}

#' @export
isBitJSON <- function(json, pattern=NULL) {
  stopifnot(isTruthyChar(json), is.null(pattern) | isTruthyChar(pattern))
  return(grepl('\\[("(0?0|0?1){1,2}",){7,}"(0?0|0?1){1,2}"\\]',
               json,
               perl=TRUE))
}

#' Serialize an R object to bit JSON
#' 
#' 
#' 
#' @export
toBitJSON <- function(x,
                      file=NULL,
                      remote=NULL,
                      pattern=NULL) {
  stopifnot(isRData(x), 
            is.null(file) | is.null(remote),
            is.null(file) | isValidFileName(file),
            is.null(remote) | isValidRemoteName(remote),
            is.null(pattern) | isTruthyChar(pattern))
  z <- jsonlite::toJSON(as.character(serializeDataToBits(x)))
  if (is.null(file) && is.null(remote)) {
    return(z)
  } else if (is.character(file)) {
    cat(z, file=file)
    return(invisible(z))
  } else if (is.character(remote)) {
    # curl PUT json to remote
    return(invisible(z))
  }
}

#' Unserialize an R object encoded as bit JSON
#' 
#' 
#' 
#' @export
fromBitJSON <- function(x, pattern=NULL) {
  stopifnot(isBitJSON(x), is.null(pattern) | isTruthyChar(pattern))
  return(unserialize(packBits(as.integer(jsonlite::fromJSON(x)), type='raw')))
}