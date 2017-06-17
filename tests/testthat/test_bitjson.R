# test_bitjson

testthat::context('consistency and predicate')

testthat::test_that('data consistency', {

  # mapped still identical
  testthat::expect_identical(fromBitJSON(toBitJSON(datasets::iris)),
                             datasets::iris)
  testthat::expect_identical(fromBitJSON(toBitJSON(datasets::islands)),
                             datasets::islands)
  testthat::expect_identical(fromBitJSON(toBitJSON(datasets::PlantGrowth)),
                             datasets::PlantGrowth)
  testthat::expect_identical(fromBitJSON(toBitJSON(datasets::Nile)),
                             datasets::Nile)
  testthat::expect_identical(fromBitJSON(toBitJSON(419L)), 
                             419L)
  
})

testthat::test_that('predicate function', {

  # setup
  valid.bits <- toBitJSON(419L)
  invalid.bits <- '[0,1,0,0,"0",1,0,1]'
  nclosed.bits <- paste0('{"acab":', toBitJSON(1L), ',"haha":[419]}')

  # match
  testthat::expect_identical(looksLikeBitJSON(valid.bits),
                             TRUE)
  testthat::expect_identical(looksLikeBitJSON(gsub('"', '', invalid.bits)),
                             TRUE)

  # nomatch
  testthat::expect_identical(looksLikeBitJSON(invalid.bits),
                             FALSE)
  testthat::expect_identical(looksLikeBitJSON(nclosed.bits),
                             FALSE)
  testthat::expect_identical(looksLikeBitJSON('[0,1,2,3,4]'),
                             FALSE)

})
