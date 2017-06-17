# test_compression

testthat::context('de/compression')

testthat::test_that('lossless de/compression', {
  
  # compression
  testthat::expect_identical(compressBits(as.integer(c(0,1,1,0,0,0,0,1,1,0))),
                             as.integer(c(0,2,1,4,0,2,1,0)))
  
  # decompression
  testthat::expect_identical(decompressBits(as.integer(c(0,2,1,4,0,2,1,0))),
                             as.integer(c(0,1,1,0,0,0,0,1,1,0)))
  
})