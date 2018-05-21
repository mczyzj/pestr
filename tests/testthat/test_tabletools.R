context("EPPO tabletools functions")
library(pestr)

test_that("Test that f returns correct structure from database", {
  testing_names <- eppo_names_tables('Xylella')
  result_names <- eppo_tabletools_names(testing_names)

  expect_is(result_names, 'list')
  expect_is(result_names[[1]], 'data.frame')
  expect_is(result_names[[2]], 'data.frame')
})

test_that("Test that f creates correct long data frame", {
  testing_names <- eppo_names_tables('Xylella')
  result_names <- eppo_tabletools_names(testing_names)
  expect_equal(sort(unique(result_names[[1]]$Preferred_name)),
               sort(dplyr::filter(testing_names$all_associated_names,
                             preferred == 1)$fullname))
  expect_equal(sort(subset(result_names[[1]],
                           Other_names != "none")$Other_names),
                 sort(dplyr::filter(testing_names$all_associated_names,
                                    preferred == 0)$fullname))
  expect_equal(sort(unique(result_names[[1]]$Name_type)),
               c('Other languages', 'Preferred', 'Synonym'))
})

test_that("Test that f creates correct condensed data frame", {
  testing_names <- eppo_names_tables(c('Xylella', 'Cydia packardi'))
  result_names <- eppo_tabletools_names(testing_names)
  cydia_test <- testing_names[[4]] %>%
    dplyr::filter(eppocode == 'LASPPA', preferred == 0) %>%
    dplyr::mutate(Other = ifelse(codelang == 'la', 'Synonym', 'Other languages')) %>%
    dplyr::arrange(desc(Other), fullname) %>%
    dplyr::select(fullname, Other) %>%
    dplyr::group_by(Other) %>%
    dplyr::mutate(Other_names = paste(fullname, collapse = ', ')) %>%
    dplyr::mutate(Other_names = paste(Other, Other_names, sep = ': ')) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(Other_names) %>%
    dplyr::mutate(Other_names = paste(Other_names, collapse = '; ')) %>%
    dplyr::distinct()
  expect_equal(result_names[[2]][1,4], cydia_test[[1]])
})

