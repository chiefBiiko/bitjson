# bitjson

#' Serialize an R object to bit JSON
#'
#'
#'
#'
toBitJSON <- function(x, file=NULL) {
  stopifnot(is.object(x) | is.vector(x) | is.atomic(x),
            is.null(file) | 
              (is.character(file) && nchar(file) > 0L))
  y <- rawToBits(serialize(x, connection=NULL))
  z <- jsonlite::toJSON(as.character(y))
  if (is.null(file)) {
    return(z)
  } else {
    cat(z, file=file)
    return(invisible(z))
  }
}

#' Unserialize an R object encoded as bit JSON
#'
#'
#'
#'
fromBitJSON <- function(x) {
  stopifnot(is.character(x), nchar(x) > 0L)
  return(unserialize(packBits(as.integer(jsonlite::fromJSON(x)), type='raw')))
}