context("pestr helper functions")
library("pestr")
library("dplyr")
library("rlang")

test_that("check_eppocodes recognize wrong and correct codes", {

  all_good <- c("ABIAL", "XYLEFA", "1GRAF")
  all_wrong <- c("abial", "123456", "XYLEFAZ", "xylEFA", "1graf", "1GrAf")
  mixed_type <- c("ABIAL", "XYLEFAZ")

  expect_message(check_eppocodes(all_wrong))
  expect_message(check_eppocodes(mixed_type))
  expect_silent(check_eppocodes(all_good))

  expect_equal(check_eppocodes(mixed_type), c("ABIAL"))
  expect_equal(check_eppocodes(all_good), c("ABIAL", "XYLEFA", "1GRAF"))
  expect_null(check_eppocodes(all_wrong))
})
