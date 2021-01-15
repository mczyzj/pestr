context("EPPO API helpers")
library("pestr")

#test if eppo_rest_download is downloading resuts into list
test_that('Test that eppo_rest_download is downloading correctly', {
  skip_on_travis()
  skip_on_cran()
  skip('Only for use locally with proper token.') #comment out to test
  eppo_token <- create_eppo_token('')
  eppocode_1 <- "XYLEFA"
  expect_true(is.list(eppo_rest_download(eppocode_1, "hosts", eppo_token)))
})

#test if eppo_csv_download is downloading results into list

test_that('Test that eppo_csc_download is downloading correctly', {
  skip_on_travis()
  skip_on_cran()
  skip("Only for local use due to downloading files") #comment out to test
  eppocode_1 <- "XYLEFA"
  expect_true(is.list(eppo_csv_download(eppocode_1)))
})

#test if eppo_try_urls returns error when there is no token

test_that('Test that eppo_try_urls returns error', {
  skip_on_travis()
  skip_on_cran()
  eppo_token <- create_eppo_token('')
  eppocode_1 <- "XYLEFA"
  test_link <- paste0('https://data.eppo.int/api/rest/1.0/taxon/',
                      "XYLEFA", "/hosts", eppo_token)
  expect_message(eppo_try_urls(test_link))
}

)
