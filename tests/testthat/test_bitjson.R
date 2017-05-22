# test_bitjson

testthat::context('mappings and predicates')

testthat::test_that('mapping preserves data integrity', {
  # setup
  saka <- data.frame(a=1L:2L, b=c('voo', 'doo'))
  # mapped still identical
  testthat::expect_identical(fromBitJSON(toBitJSON(saka)), saka)
})

testthat::test_that('predicate function works', {
  # setup
  valid.bits <- toBitJSON(419L)
  invalid.bits <- '[\'0\',"1","0","0",\'0\',"1","0","1"]'
  # match
  testthat::expect_identical(isBitJSON(valid.bits), TRUE)
  # nomatch
  testthat::expect_identical(isBitJSON(invalid.bits), FALSE)
})