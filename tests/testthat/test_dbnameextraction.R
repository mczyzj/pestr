context("EPPO database functions")
library("pestr")
library("dplyr")

test_that("Test that f returns correct names from database", {
  testNames <- c('Cydia packardi', 'noPest', 'cherry virus', 'noPest2')
  resultNames <- list(exist_in_DB = c('Cydia packardi',
                                      'Cherry little cherry virus',
                                      'Epirus cherry virus',
                                      'Cherry little cherry virus',
                                      'Cherry virus A',
                                      'Little cherry virus 1',
                                      'Little cherry virus 2',
                                      'Little cherry virus 3'),
                      not_in_DB = c('noPest', 'noPest2'))
  preferredNames <- c('Grapholita packardi',
                      'Epirus cherry virus',
                      'Cherry virus A',
                      'Little cherry virus 1',
                      'Little cherry virus 2')

  expect_equal(eppo_names_tables(testNames)[[1]]$fullname, resultNames[[1]])
  expect_equal(eppo_names_tables(testNames)[[2]], resultNames[[2]])
  expect_equal(eppo_names_tables(testNames)[[3]]$fullname, preferredNames)
})

test_that("Test that f returns empty tables and non empty vector of
          incorect names when all names in query are invalid", {

  expect_is(eppo_names_tables('nonSenS12'), 'list')
  expect_is(eppo_names_tables('nonSenS12')$exist_in_DB, 'data.frame')
  expect_is(eppo_names_tables('nonSenS12')$not_in_DB, 'character')
  expect_is(eppo_names_tables('nonSenS12')$pref_names, 'data.frame')
  expect_is(eppo_names_tables('nonSenS12')$all_associated_names, 'data.frame')

  expect_equal(dim(eppo_names_tables('nonSenS12')[[1]]), c(0L, 2L))
  expect_equal(eppo_names_tables('nonSenS12')[[2]], 'nonSenS12')
  expect_equal(dim(eppo_names_tables('nonSenS12')[[3]]), c(0L, 3L))
  expect_equal(dim(eppo_names_tables('nonSenS12')[[4]]), c(0L, 5L))
})

