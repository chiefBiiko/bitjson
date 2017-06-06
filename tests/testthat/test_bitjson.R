# test_bitjson

testthat::context('mappings and predicates')

testthat::test_that('mapping preserves data integrity', {
  # setup
  saka <- data.frame(a=1L:2L, b=c('voo', 'doo'))
  # mapped still identical
  testthat::expect_identical(fromBitJSON(toBitJSON(saka)), saka)
})

testthat::test_that('predicate functions work', {
  
  # setup
  valid.bits <- toBitJSON(419L)
  invalid.bits <- '[0,1,0,0,\'0\',1,0,1]'
  nclosed.bits <- paste0('{"acab":', toBitJSON(1L, compress=FALSE), ',"haha":[419]}')
  multi <- paste0('[', 
                  toBitJSON(2L, compress=FALSE), ',', 
                  jsonlite::toJSON(99999L), ',', 
                  toBitJSON(419L, compress=FALSE), 
                  ']')
  threesix <- serializeToBits(36L)
  sakawa <- serializeToBits(list(4L, 1L, 9L))
  
  # match
  testthat::expect_identical(isBitJSON(valid.bits, compressed=TRUE), 
                             TRUE)
  
  # nomatch
  testthat::expect_identical(isBitJSON(invalid.bits, compressed=FALSE), 
                             FALSE)
  testthat::expect_identical(isBitJSON(nclosed.bits, compressed=FALSE), 
                             FALSE)
  testthat::expect_identical(isBitJSON(multi, compressed=FALSE), FALSE)
  
  # contains
  testthat::expect_identical(containsBitJSON(nclosed.bits), TRUE)
  testthat::expect_identical(containsBitJSON(multi), TRUE)
  
  # extract
  testthat::expect_identical(isBitJSON(extractBitJSON(nclosed.bits, compressed=FALSE)), 
                             TRUE)
  testthat::expect_identical(sapply(extractBitJSON(multi, compressed=FALSE), 
                                    isBitJSON, 
                                    compressed=FALSE,
                                    USE.NAMES=FALSE), 
                             c(TRUE, TRUE))
  
})