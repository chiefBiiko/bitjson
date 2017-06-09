# bitjson

#' Is a JSON string a literal bit array or a bit array compressed via chief run-
#' length encoding?
#' 
#' TODO: better sequence matching !!!
#' 
#' @param json JSON string.
#' @return Logical.
#'
#' @seealso \code{\link{toBitJSON}} \code{\link{fromBitJSON}}
#'
#' @export
isBitJSON <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl(paste0('(?:^\\[((?:0|1),)*(?:0|1)\\]$)|', 
                      '(?:^\\[(?:(?:(?:0|1),)*\\d+,(?:0|1),?)+(?:0|1)*\\]$)'), 
               json, 
               perl=TRUE))
}

# Does a JSON string contain bit json?
#
# @param json JSON string.
# @return Logical.
#
# @export 
#containsBitJSON <- function(json) {
#  stopifnot(isTruthyChr(json))
#  return(grepl(paste0('(?:^.*\\[((?:0|1),)*(?:0|1)\\].*$)|', 
#                      '(?:^.*\\[(?:(?:(?:0|1),)*\\d+,(?:0|1),?)+(?:0|1)*\\].*$)'), 
#               json, 
#               perl=TRUE))
#}

# Extract bit JSON from a JSON string
#
# @param json JSON string.
# @return JSON string.
#
# @export 
#extractBitJSON <- function(json) {
#  stopifnot(isTruthyChr(json))
#  rex <- paste0('(?:\\[((?:0|1),)*(?:0|1)\\])|', 
#                '(?:\\[(?:(?:(?:0|1),)*\\d+,(?:0|1),?)+(?:0|1)*\\])')
#  rtn <- regmatches(json, gregexpr(rex, json, perl=TRUE))[[1]]
#  # serve
#  return(structure(rtn, class='json'))
#}

#' Serialize an R object to bit JSON
#' 
#' @param x Any R object.
#' @param file File name to which to write bit JSON.
#' @param compress Compress the return bit array to a chief run-length encoded 
#' integer array?
#' @return Bit json.
#' 
#' @seealso \code{\link{isBitJSON}} \code{\link{fromBitJSON}}
#' 
#' @export
toBitJSON <- function(x, file=NULL, compress=TRUE) {
  stopifnot(isRData(x) | is.function(x), 
            is.null(file) | isTruthyChr(file),
            is.logical(compress))
  if (compress) {
    z <- jsonlite::toJSON(serializeToBits(x, compress=compress))
  } else if (!compress) {
    z <- jsonlite::toJSON(serializeToBits(x, compress=compress))
  }
  if (is.null(file)) {
    return(z)
  } else if (is.character(file)) {
    cat(z, file=file, append=TRUE)
    return(invisible(z))
  }
}

#' Unserialize an R object encoded as bit JSON
#' 
#' @param x Bit JSON.
#' @param compressed Is the bit JSON array compressed via chief run-length 
#' encoding?
#' @return R object.
#' 
#' @seealso \code{\link{toBitJSON}} \code{\link{isBitJSON}}
#' 
#' @export
fromBitJSON <- function(x, compressed=TRUE) {
  stopifnot(isBitJSON(x), is.logical(compressed))
  if (compressed) {
    return(unSerializeFromBits(as.integer(jsonlite::fromJSON(x)), 
                               compressed=compressed))
  } else if (!compressed) {
    return(unSerializeFromBits(as.integer(jsonlite::fromJSON(x)), 
                               compressed=compressed))
  }
}