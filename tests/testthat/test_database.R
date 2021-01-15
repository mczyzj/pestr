context("EPPO database connect")
library("pestr")

test_that("Test that there is database", {
 expect_message(eppo_database_check())
 expect_message(eppo_database_check('/Database'), 'file does not exist')
 expect_message(eppo_database_check(filename = 'emt'), 'file does not exist')
 expect_message(eppo_database_check('/Database', ''), 'file does not exist')
 expect_message(eppo_database_check('/Database', 'emt'), 'file does not exist')
})

test_that("Test that connects to databes if exist", {
  expect_message(eppo_database_connect(),
                 'Your connection to EPPO SQLite database is etablished')
  expect_message(eppo_database_connect(filepath = '/no_db'), 'no such database')
  expect_message(eppo_database_connect(filename = '/nb@'), 'no such database')
  expect_is(eppo_database_connect(), 'SQLiteConnection')
})
