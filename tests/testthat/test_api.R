context("EPPO API helpers")
library(pestr)

test_that('Test that eppo_rest_download is downloading correctly', {
  skip_on_travis()
  skip_on_cran()
  skip('Only for use locally with proper token.') #comment out to test
  create_eppo_token('')
  eppocode_1 <- "XYLEFA"
  expect_true(is.list(eppo_rest_download(eppocode_1, "hosts", eppo_token)))
})

# rm(eppo_token, envir = globalenv())
