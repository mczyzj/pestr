context("EPPO database functions")
library(pestr)

test_that("Test that f returns correct names from database", {
  testNames <- c('Cydia packardi', 'noPest', 'cherry virus', 'noPest2')
  resultNames <- list(exist_in_DB = c("Cydia packardi",
                                      "Cherry little cherry virus",
                                      "Epirus cherry virus",
                                      "Cherry little cherry virus",
                                      "Cherry virus A",
                                      "Little cherry virus 1",
                                      "Little cherry virus 2"),
                      not_in_DB = c('noPest', 'noPest2'))
  expect_is(DBI::dbGetQuery(RSQLite::dbConnect(RSQLite::SQLite(),
                                               dbname = 'eppocodes.sqlite'),
                            "SELECT fullname FROM t_names WHERE fullname LIKE 'Cydia packardi'"), 'data.frame')
  expect_is(eppo_organism_check(testNames), 'list')
  expect_is(eppo_organism_check(testNames)$exist_in_DB, 'data.frame')
  expect_is(eppo_organism_check(testNames)$not_in_DB, 'character')
  expect_equal(eppo_organism_check(testNames)[[1]]$fullname, resultNames[[1]])
  expect_equal(eppo_organism_check(testNames)[[2]], resultNames[[2]])
})
