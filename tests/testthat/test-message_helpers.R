context("Messages")
library("pestr")

test_that("msg_helper returns character string", {
  expect_type(msg_helper("wrong_token"), "character")
  expect_type(msg_helper("db_time", "eppocodes.sqlite"), "character")
  expect_null(msg_helper("aaa"))
})
