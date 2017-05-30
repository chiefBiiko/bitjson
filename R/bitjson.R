# bitjson

#' @internal
serializeDataToBits <- function(x) {
  return(as.integer(rawToBits(serialize(x, connection=NULL))))
}

#' @export
isBitJSON <- function(json, pattern=NULL) {
  stopifnot(isTruthyChr(json), is.null(pattern) | isTruthyChr(pattern))
  return(grepl('^\\[("?(?:0?0|0?1)"?,){7,}"?(?:0?0|0?1)"?\\]$', 
               json, 
               perl=TRUE))
}

#' @export
containsBitJSON <- function(json, pattern=NULL) {
  stopifnot(isTruthyChr(json), is.null(pattern) | isTruthyChr(pattern))
  return(grepl('^.*\\[("?(?:0?0|0?1)"?,){7,}"?(?:0?0|0?1)"?\\].*$', 
               json, 
               perl=TRUE))
}

#' @export 
extractBitJSON <- function(json) {
  stopifnot(isTruthyChr(json))
  rtn <- regmatches(json,
                    gregexpr('\\[(?:("?(?:0?0|0?1)"?,){7,}"?(?:0?0|0?1)"?)+\\]', 
                             json, 
                             perl=TRUE))[[1]]
  return(structure(rtn, class='json'))
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
            is.null(file) | isTruthyChr(file),
            is.null(remote) | isValidRemoteName(remote),
            is.null(pattern) | isTruthyChr(pattern))
  z <- jsonlite::toJSON(serializeDataToBits(x))
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
  stopifnot(isBitJSON(x), is.null(pattern) | isTruthyChr(pattern))
  return(unserialize(packBits(as.integer(jsonlite::fromJSON(x)), type='raw')))
}