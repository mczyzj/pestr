context("EPPO token")
library("pestr")

test_that('Test that eppo_token is S3 class named pestr_token', {
  expect_is(create_eppo_token('abc123'), 'pestr_token')
})

test_that("Test that token is a string", {
  expect_is(create_eppo_token(123L), 'character')
  expect_is(create_eppo_token('abc'), 'character')
})

test_that("Test that token has proper format", {
  expect_message(create_eppo_token('aa@-'), 'forbiden characters')
  expect_message(create_eppo_token('AAZ'), 'forbiden characters')
  expect_message(create_eppo_token('ghij'), 'forbiden characters')
})
