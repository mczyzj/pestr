context("Tests that")
library("pestr")

test_that("eppo_citation returns bibtex citations of EPPO resources", {

  #EPPO Global Database citation
  eppo_gd_cit <- c(
    "@online{eppoGD,",
    "    title = {EPPO Global Database},",
    "    author = {{EPPO}},",
    "    organization = {European and Mediterranean Plant Protection Organization},",
    "    address = {Paris, France},",
    "    year = {2021},",
    "    url = {https://gd.eppo.int/},",
    "    urldate = {},",
    "  }"
    )
  #EPPO Data Services citation
  eppo_ds_cit <- c(
    "@online{eppoDS,",
    "    title = {EPPO Data Services},",
    "    author = {{EPPO}},",
    "    organization = {European and Mediterranean Plant Protection Organization},",
    "    address = {Paris, France},",
    "    year = {2021},",
    "    url = {https://data.eppo.int/},",
    "    urldate = {},",
    "  }"
  )

  #Capture function with different arguments output
  cite_ds_output <- capture.output(eppo_citation("data_services"))
  cite_gd_output <- capture.output(eppo_citation("global_database"))
  cite_both_output <- capture.output(eppo_citation("both"))

  #Check if function output matches the proprer citation
  expect_equal(cite_ds_output, eppo_ds_cit)
  expect_equal(cite_gd_output, eppo_gd_cit)
  expect_equal(cite_both_output,
               c(eppo_ds_cit, "", eppo_gd_cit))

})

test_that("eppo_citation returns informative messages when arguments are incorrect", {

  #Check messages if one of the arguments is incorrect
  expect_message(eppo_citation("xyz"),
                 "Argument xyz is incorrect")
  expect_message(eppo_citation(c("xyz", "abc")),
                 "Cite argument has lenght != 1")
  expect_message(eppo_citation("global_database", 123),
                 "Please use character string for name of output")

})

test_that("eppo_citation prints remeber message", {
  expect_message(eppo_citation("global_database"), "adjust citation key")
})


test_that("eppo_citation saves output and print file name", {
  skip_on_cran()
  skip_on_travis()
  skip("Only for local use due to creating files") #comment out to test

  #use output argument to check if the file is crated
  eppo_citation("both", "test_bibliography.bib")

  expect_true(file.exists("test_bibliography.bib"))

  unlink("test_bibliography.bib")

})
