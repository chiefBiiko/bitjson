# bitjson utils


#' Serialize any R object to a bit vector.
#' 
#' @param x Any R object.
#' @return Bit vector.
#'
#' @internal
isTruthyChr <- function(string) {
  if (is.character(string) && nchar(string) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Is the remote a http-accessible file?
#' 
#' @param remote Character vector of length 1.
#' @return Logical.
#'
#' @internal
isValidRemoteName <- function(remote) {
  if (is.character(remote) && 
      grepl('http(s)?://.+\\..+', remote, perl=TRUE)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Is an object a data object?
#' 
#' @param x Any R object.
#' @return Logical.
#'
#' @internal 
isRData <- function(x) {
  if (is.object(x) | is.vector(x) | is.atomic(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Serialize any R object to a bit vector.
#' 
#' @param x Any R object.
#' @return Compressed bit vector.
#'
#' @internal
serializeToBits <- function(x, compress=TRUE) {
  stopifnot(isRData(x) | is.function(x))
  if (compress) {
    return(compressBits(as.integer(rawToBits(serialize(x, connection=NULL)))))
  } else if (!compress) {
    return(as.integer(rawToBits(serialize(x, connection=NULL))))
  }
}

#' Unserialize an R object from a bit vector.
#' 
#' @param x Compressed bit vector.
#' @return R object.
#'
#' @internal
unSerializeFromBits <- function(x, compressed=TRUE) {
  stopifnot(is.integer(x), is.logical(compressed))
  if (compressed) {
    return(unserialize(packBits(decompressBits(x), type='raw')))
  } else if (!compressed) {
    return(unserialize(packBits(x, type='raw')))
  }
}