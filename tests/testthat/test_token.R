context("EPPO token")
library(pestr)

test_that('Test that eppo_token is S3 class named pestr_token', {
  expect_equal(pryr::otype(create_eppo_token('abc123')), 'S3')
  expect_is(create_eppo_token('abc123'), 'pestr_token')
})

test_that("Test that token is a string", {
  expect_is(create_eppo_token(123L), 'character')
  expect_is(create_eppo_token('abc'), 'character')
})

test_that("Test that token has proper format", {
  expect_warning(create_eppo_token('aa@-'), 'forbiden characters')
  expect_warning(create_eppo_token('AAZ'), 'forbiden characters')
  expect_warning(create_eppo_token('ghij'), 'forbiden characters')
})

test_that("Test that function creates global variable eppo_token", {
  expect_true(create_eppo_token('abc123') == '?authtoken=abc123',
              exists('eppo_token'))
  expect_false(create_eppo_token('abc123') == '?authtoken=123abc',
               exists('eppo_token'))
  expect_equal(create_eppo_token('Z@a1') == '123abc', !exists('eppo_token'))
})

rm(eppo_token, envir = globalenv())
