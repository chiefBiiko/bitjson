# bitjson

#' Does a R object look like a literal bitjson array or a bitjson array
#' compressed via chief run-length encoding?
#'
#' @param x Any R object.
#' @return Logical.
#'
#' @seealso \code{\link{toBitJSON}} \code{\link{fromBitJSON}}
#'
#' @export
looksLikeBitJSON <- function(x) {
  if (!is.character(x)) return(FALSE)
  return(grepl('^\\[(?:(?:0|1),)*(?:0|1)\\]$', x, perl=TRUE) ||
           grepl('^\\[(?:(?:0|1),)*(?:\\d+,)+(?:0|1)\\]$', x, perl=TRUE))
}

#' Serialize an R object to bit JSON
#'
#' @param x Any R object.
#' @param file Character. File name to which to write bit JSON.
#' @param append Logical. When writing to a file with \code{cat} overwrite
#' or append to existing contents?
#' @param compress Logical. Compress the return bit array to a chief
#' run-length encoded integer array?
#' @param ... Further arguments passed on to \code{jsonlite::toJSON}.
#' @return JSON.
#'
#' @seealso \code{\link{looksLikeBitJSON}} \code{\link{fromBitJSON}}
#'
#' @export
toBitJSON <- function(x, file=NULL, append=FALSE, compress=TRUE, ...) {
  stopifnot(isDataObject(x) | is.function(x),
            is.null(file) | isTruthyChr(file),
            is.logical(append), is.logical(compress))
  z <- jsonlite::toJSON(serializeToBits(x, compress=compress), ...)
  if (is.null(file)) {
    return(z)
  } else {
    cat(z, file=file, append=append)
    return(invisible(z))
  }
}

#' Unserialize an R object encoded as bit JSON
#'
#' @param x Character. Bit JSON string or file reference.
#' @param compressed LogicalIs the bit JSON array compressed via chief
#' run-length encoding?
#' @param ... Further arguments passed on to \code{jsonlite::fromJSON}.
#' @return R object.
#'
#' @seealso \code{\link{toBitJSON}} \code{\link{looksLikeBitJSON}}
#'
#' @export
fromBitJSON <- function(x, compressed=TRUE, ...) {
  stopifnot(isTruthyChr(x), is.logical(compressed))
  return(unSerializeFromBits(as.integer(jsonlite::fromJSON(x, ...)),
                             compressed=compressed))
}
