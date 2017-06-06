# bitjson

#' Is a JSON string a bit array?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @export
isBitJSON <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl(paste0('(?:^\\[((?:0|1),)*(?:0|1)\\]$)|', 
                      '(?:^\\[(?:(?:(?:0|1),)*\\d+,(?:0|1),?)+(?:0|1)*\\]$)'), 
               json, 
               perl=TRUE))
}

#' Does a JSON string contain bit json?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @export 
containsBitJSON <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl(paste0('(?:^.*\\[((?:0|1),)*(?:0|1)\\].*$)|', 
                      '(?:^.*\\[(?:(?:(?:0|1),)*\\d+,(?:0|1),?)+(?:0|1)*\\].*$)'), 
               json, 
               perl=TRUE))
}

#' Extract bit JSON from a JSON string
#'
#' @param json JSON string.
#' @return JSON string.
#'
#' @export 
extractBitJSON <- function(json) {
  stopifnot(isTruthyChr(json))
  rex <- paste0('(?:\\[((?:0|1),)*(?:0|1)\\])|', 
                '(?:\\[(?:(?:(?:0|1),)*\\d+,(?:0|1),?)+(?:0|1)*\\])')
  rtn <- regmatches(json, gregexpr(rex, json, perl=TRUE))[[1]]
  # serve
  return(structure(rtn, class='json'))
}

#' Serialize an R object to bit JSON
#' 
#' @param x Any R object.
#' @param file File name to which to write bit JSON.
#' @param compress Compress the bit array to a run length encoded digit array?
#' @return Bit json.
#' 
#' @export
toBitJSON <- function(x, file=NULL, compress=TRUE) {
  stopifnot(isRData(x) | is.function(x), 
            is.null(file) | isTruthyChr(file),
            is.logical(compress))
  if (compress) {
    z <- jsonlite::toJSON(serializeToBits(x))
  } else if (!compress) {
    z <- jsonlite::toJSON(serializeToBits(x, compress=FALSE))
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
#' @return R object.
#' 
#' @export
fromBitJSON <- function(x) {
  stopifnot(isBitJSON(x))
  return(unSerializeFromBits(as.integer(jsonlite::fromJSON(x))))
}