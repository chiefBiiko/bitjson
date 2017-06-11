# bitjson compression

# Compress a bit vector using chief run-length encoding
#
# @param bits Bit vector.
# @return Integer vector.
#
# @keywords internal
compressBits <- function(bits) {
  stopifnot(is.integer(bits))
  if (length(bits) < 3L) return(bits)
  # setup
  x <- vector('integer')  # how 2 predefine length correctly?
  count <- 0L
  # map compress
  for (i in seq(length(bits))) {
    # identify digit change
    if (!bits[i] %in% bits[i - 1L]) {
      # record digit count
      if (count == 1L) {
        x <- append(x, bits[i - 1L])
      } else if (count > 1L) {
        x <- append(x, c(count, bits[i - 1L]))
      }
      count <- 0L        # reset digit count
    }
    count <- count + 1L  # increment digit count
  }
  # consume remainder
  if (count == 1L) {
    x <- append(x, bits[length(bits) - 1L])
  } else if (count > 1L) {
    x <- append(x, c(count, bits[length(bits) - 1L]))
  }
  # serve compressed
  return(x)
}

# Decompress a chief run-length encoded integer vector
#
# @param encbits Integer vector.
# @return Bit vector.
#
# @keywords internal
decompressBits <- function(encbits) {
  stopifnot(is.integer(encbits))
  if (length(encbits) < 2L) return(encbits)
  # setup
  litbit <- c(0L, 1L)
  # map decompress
  dcdbits <- octostep::octostep(as.list(encbits), function(pre, cur, nxt) {
    if (cur > 1L) {
      # case cur is digit count predecessor
      NULL
    } else if (pre > 1L && cur %in% litbit) {
      # case encoded digit run 
      rep(cur, pre)
    } else if ((is.null(pre) && cur %in% litbit) | 
               (!is.null(pre) && pre %in% litbit && cur %in% litbit)) { 
      # case unencoded literal bit (run of length 1)
      cur
    }
  })
  # recombine
  rtn <- unlist(dcdbits[!sapply(dcdbits, is.null)])
  # serve decompressed
  return(rtn)
}