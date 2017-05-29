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
  invalid.bits <- '[\'0\',"1","0","0",\'0\',"1","0","1"]'
  nclosed.bits <- paste0('{"acab":', toBitJSON(1L), ',"haha":[419]}')
  multi <- paste0('[', 
                  toBitJSON(2L), ',', 
                  jsonlite::toJSON(99999L), ',', 
                  toBitJSON(419L), 
                  ']')
  
  # match
  testthat::expect_identical(isBitJSON(valid.bits), TRUE)
  
  # nomatch
  testthat::expect_identical(isBitJSON(invalid.bits), FALSE)
  testthat::expect_identical(isBitJSON(nclosed.bits), FALSE)
  
  # contains
  testthat::expect_identical(containsBitJSON(nclosed.bits), TRUE)
  
  # extract
  testthat::expect_identical(isBitJSON(extractBitJSON(nclosed.bits)), TRUE)

})