# bitjson utils

#' Is character vector with number of characters >= 1?
#'
#' @param x R object.
#' @return Logical.
#'
#' @keywords internal
isTruthyChr <- function(x) {
  if (is.character(x) && nchar(x) > 0L) {
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
#' @details Any R object for which \code{is.language} is \code{TRUE}
#' [calls, expressions, names/symbols] are not considered data objects
#' in this context.
#'
#' @keywords internal
isDataObject <- function(x) {
  if (is.vector(x) | is.atomic(x) | is.object(x) | isS4(x) |
      is.environment(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Serialize any R object to a bit vector.
#'
#' @param x Any R object.
#' @param Logical. compress Should the bit vector be compressed via chief
#' run-length encoding?
#' @return Integer. Compressed bit vector.
#'
#' @keywords internal
serializeToBits <- function(x, compress=TRUE) {
  stopifnot(isDataObject(x) | is.function(x))
  if (compress) {
    return(compressBits(as.integer(rawToBits(serialize(x, connection=NULL)))))
  } else {
    return(as.integer(rawToBits(serialize(x, connection=NULL))))
  }
}

#' Unserialize an R object from a bit vector.
#'
#' @param x Integer. Compressed bit vector.
#' @return R object.
#'
#' @keywords internal
unSerializeFromBits <- function(x, compressed=TRUE) {
  stopifnot(is.integer(x), is.logical(compressed))
  if (compressed) {
    return(unserialize(packBits(decompressBits(x), type='raw')))
  } else {
    return(unserialize(packBits(x, type='raw')))
  }
}
