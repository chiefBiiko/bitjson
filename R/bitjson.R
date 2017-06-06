# bitjson

#' Is a JSON string a bit array?
#'
#' @param json JSON string.
#' @param compressed Is \code{json} compressed?
#' @return Logical.
#'
#' @export
isBitJSON <- function(json, compressed=FALSE) {
  stopifnot(isTruthyChr(json), is.logical(compressed))
  if (!compressed) {
    return(grepl('^\\[("?(?:0?0|0?1)"?,)*"?(?:0?0|0?1)"?\\]$', 
                 json, 
                 perl=TRUE))
  } else if (compressed) {
    return(grepl('^\\[("?\\d+"?,)*"?\\d+"?\\]$', 
                 json, 
                 perl=TRUE))
  }
}

#' Does a JSON string contain bit json?
#'
#' @param json JSON string.
#' @param compressed Is \code{json} compressed?
#' @return Logical.
#'
#' @export 
containsBitJSON <- function(json, compressed=FALSE) {
  stopifnot(isTruthyChr(json), is.logical(compressed))
  if (!compressed) {
    return(grepl('^.*\\[("?(?:0?0|0?1)"?,)*"?(?:0?0|0?1)"?\\].*$', 
                 json, 
                 perl=TRUE))
  } else if (compressed) {
    return(grepl('^.*\\[("?\\d+"?,)*"?\\d+"?\\].*$', 
                 json, 
                 perl=TRUE))
  }
}

#' Extract bit JSON from a JSON string
#'
#' @param json JSON string.
#' @param compressed Is \code{json} compressed?
#' @return JSON string.
#'
#' @export 
extractBitJSON <- function(json, compressed=FALSE) {
  stopifnot(isTruthyChr(json), is.logical(compressed))
  if (!compressed) {
    rtn <- regmatches(json,
                      gregexpr('\\[(?:("?(?:0?0|0?1)"?,)*"?(?:0?0|0?1)"?)+\\]', 
                               json, 
                               perl=TRUE))[[1]]
    return(structure(rtn, class='json'))
  } else if (compressed) {
    rtn <- regmatches(json,
                      gregexpr('\\[(?:("?\\d+"?,)*"?\\d+"?)+\\]', 
                               json, 
                               perl=TRUE))[[1]]
    return(structure(rtn, class='json'))
  }
}

#' Serialize an R object to bit JSON
#' 
#' @param x Any R object.
#' @param file File name to which to write bit JSON.
#' @param remote Remote name to which to PUT bit JSON.
#' @param compress Compress the bit array to a run length encoded digit array?
#' @return Bit json.
#' 
#' @export
toBitJSON <- function(x,
                      file=NULL,
                      remote=NULL,
                      compress=TRUE) {
  stopifnot(isRData(x) | is.function(x), 
            is.null(file) | is.null(remote),
            is.null(file) | isTruthyChr(file),
            is.null(remote) | isValidRemoteName(remote),
            is.logical(compress))
  if (compress) {
    z <- jsonlite::toJSON(serializeToBits(x))
  } else if (!compress) {
    z <- jsonlite::toJSON(serializeToBits(x, compress=FALSE))
  }
  if (is.null(file) && is.null(remote)) {
    return(z)
  } else if (is.character(file)) {
    cat(z, file=file, append=TRUE)
    return(invisible(z))
  } else if (is.character(remote)) {
    # curl PUT json to remote
    return(invisible(z))
  }
}

#' Unserialize an R object encoded as bit JSON
#' 
#' @param x Bit JSON, JSON string containing bit JSON or remote name
#' accessible via http that contains bit JSON or JSON containg bit JSON.
#' @return R object.
#' 
#' @export
fromBitJSON <- function(x) {
  stopifnot(isValidRemoteName(x) | 
              ((isBitJSON(x) | isBitJSON(x, compressed=TRUE)) | 
                  (containsBitJSON(x) | containsBitJSON(x, compressed=TRUE))))
  if (isBitJSON(x)) {
    return(unserializeFromBits(as.integer(jsonlite::fromJSON(
      x
      ))))
  } else if (containsBitJSON(x)) {
    return(unserializeFromBits(as.integer(jsonlite::fromJSON(
      extractBitJSON(x)
      ))))
  } else if (isValidRemoteName(x)) {
    #resource <- curl GET remote
    return(unserializeFromBits(as.integer(jsonlite::fromJSON(
      extractBitJSON(resource)
    ))))
  }
}