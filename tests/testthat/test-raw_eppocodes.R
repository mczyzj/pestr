context("Local tests of API")
library("pestr")
library("dplyr")
library("rlang")

test_that("raw eppocodes gives same results as eppocodes from DB", {
  skip_on_travis()
  skip_on_cran()
  skip('Only for use locally with proper token.') #comment out to check locally
  eppo_token <- create_eppo_token('')

  #eppocodes raw
  test_full <- all_good <- c("XYLEFA", "ABIAL", "abial", "123456", "XYLEFAZ",
                             "xylEFA", "1graf", "1GrAf", "1GRAF")
  #eppocodes form DB
  test_full_n <- eppo_names_tables(c("Xylella fastidiosa subsp. piercei",
                                     "Abies alba", "Poacea"))


  test_host_t <- eppo_tabletools_hosts(test_full_n, eppo_token)
  test_cat_t <- eppo_tabletools_cat(test_full_n, eppo_token)
  test_taxo_t <- eppo_tabletools_taxo(test_full_n, eppo_token)
  test_distri_t <- eppo_tabletools_distri(test_full_n, eppo_token)
  test_pests_t <- eppo_tabletools_pests(test_full_n, eppo_token)

  test_host <- eppo_tabletools_hosts(token = eppo_token,
                                     raw_eppocodes = test_full,
                                     use_raw_codes = TRUE)
  test_cat <- eppo_tabletools_cat(token = eppo_token,
                                  raw_eppocodes = test_full,
                                  use_raw_codes = TRUE)
  test_taxo <- eppo_tabletools_taxo(token = eppo_token,
                                    raw_eppocodes = test_full,
                                    use_raw_codes = TRUE)
  test_distri <- eppo_tabletools_distri(raw_eppocodes = test_full,
                                        use_raw_codes = TRUE)
  test_pests <- eppo_tabletools_pests(token = eppo_token,
                                      raw_eppocodes = test_full,
                                      use_raw_codes = TRUE)
  #### test results

  expect_equal(test_host, test_host_t)
  expect_equal(test_cat, test_cat_t)
  expect_equal(test_taxo, test_taxo_t)
  expect_equal(test_distri, test_distri_t)
  expect_equal(test_pests, test_pests_t)

})
