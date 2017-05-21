# test_bitjson

testthat::context('data integrity')

testthat::test_that('mapping preserves data integrity', {
  # setup
  saka <- data.frame(a=1L:2L, b=c('voo', 'doo'))
  # mapped still identical
  testthat::expect_identical(fromBitJSON(toBitJSON(saka)), saka)
})