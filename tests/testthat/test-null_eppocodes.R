context("Test null eppocodes function")
library("pestr")
library("dplyr")
library("rlang")

test_that("Test that empty is substituted with NULL", {
  expect_null(null_eppocodes(character()))
  expect_equal(null_eppocodes("XYLEFA"), "XYLEFA")
})
