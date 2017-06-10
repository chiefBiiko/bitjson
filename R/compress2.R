# bitjson compression

#' Compress a bit vector using chief run-length encoding
#'
#' @param bits Bit vector.
#' @return Integer vector.
#'
#' @keywords internal
compressBits <- function(bits) {
  stopifnot(is.integer(bits))
  if (length(bits) < 3L) return(bits)
  # setup
  x <- vector('integer')  # how 2 predefine length correctly?
  # lookbehind
  prev <- bits[1L]
  # run length counter
  count <- 0L
  # map compress
  for (bit in bits) {
    # identify bit change
    if (bit != prev) {
      # record bit count
      if (count == 1L) {
        x <- append(x, prev)
      } else {  # if (count > 1L) omitted ...
        x <- append(x, c(count, prev))
      }
      # reset bit count
      count <- 0L
    }
    # increment bit count
    count <- count + 1L
    # keep bit as lookbehind
    prev <- bit
  }
  # consume remainder
  if (count == 1L) {
    x <- append(x, bits[length(bits) - 1L])
  } else {  # if (count > 1L) omitted ...
    x <- append(x, c(count, bits[length(bits) - 1L]))
  }
  # serve compressed
  return(x)
}

#' Decompress a chief run-length encoded integer vector
#'
#' @param encbits Integer vector.
#' @return Bit vector.
#'
#' @keywords internal
decompressBits <- function(encbits) {
  stopifnot(is.integer(encbits))
  if (length(encbits) < 2L) return(encbits)
  # setup
  litbit <- c(0L, 1L)
  x <- vector('integer')
  prev <- -1L
  # map decompress
  for (bit in encbits) {
    if (prev > 1L && bit %in% litbit) {
      # case encoded bit run 
      x <- append(x, rep(bit, prev))
    } else if (bit %in% litbit)  { 
      # case unencoded literal bit (run of length 1)
      x <- append(x, bit)
    }
    # keep bit as lookbehind
    prev <- bit
  }
  # serve decompressed
  return(x)
}