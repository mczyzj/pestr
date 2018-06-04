context("EPPO tabletools functions")
library(pestr)

#eppo_tabletools_names = names f
#eppo_tabletools_hosts = hosts f

test_that("Test that names f returns correct structure from database", {
  testing_names <- eppo_names_tables('Xylella')
  result_names <- eppo_tabletools_names(testing_names)

  expect_is(result_names, 'list')
  expect_is(result_names[[1]], 'data.frame')
  expect_is(result_names[[2]], 'data.frame')
})

test_that("Test that names f creates correct long data frame", {
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

test_that("Test that names f creates correct condensed data frame", {
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

test_that("Test that hosts f checks if parsed arguments are of proper class", {
  testing_names <- eppo_names_tables('Xylella')
  create_eppo_token('abc123')
  expect_message(eppo_tabletools_hosts(testing_names, 'some chars'),
                 'Please provide token created with create_eppo_token function')
})

test_that("Test that hosts f returns correct structure from database", {
  testing_names <- eppo_names_tables('Xylella')
  create_eppo_token('e3ecef2dea564abec28e9781eb3b9b94')
  result_hosts <- eppo_tabletools_hosts(testing_names, eppo_token)

  #expect_is(result_hosts, 'list')
  #expect_is(result_hosts[[1]], 'data.frame')
  #expect_is(result_hosts[[2]], 'data.frame')
})

test_that("Test that hosts f works correctly", {
  testing_names <- eppo_names_tables(c('Cydia packardi', 'Tuta absoluta'))
  create_eppo_token('e3ecef2dea564abec28e9781eb3b9b94')
  eppocodes <- testing_names[[3]]$eppocode
  api_url <- 'https://data.eppo.int/api/rest/1.0/taxon/'
  testing_urls <- paste0(api_url,
                         eppocodes,
                         '/hosts',
                         eppo_token)
  #test long table values
  testing_hosts <- lapply(testing_urls,
                          function(x) jsonlite::fromJSON(RCurl::getURL(x)))
  names(testing_hosts) <- eppocodes
  test_host_table <- lapply(testing_hosts, dplyr::bind_rows) %>%
    dplyr::bind_rows(.id = 'pest_code') %>%
    dplyr::rename(host_eppocode = eppocode, eppocode = pest_code)

  expect_equal(eppo_tabletools_hosts(testing_names, eppo_token)[[1]],
               test_host_table)
  #test compact table values
  compact_names <- test_host_table %>%
    dplyr::filter(eppocode == 'LASPPA') %>%
    dplyr::select(labelclass, full_name, eppocode) %>%
    tidyr::nest(labelclass, full_name)

    compact_names_test <- compact_names$data[[1]] %>%
      dplyr::group_by(labelclass) %>%
      dplyr::mutate(temp_names = paste(full_name, collapse = ', ')) %>%
      dplyr::distinct(temp_names) %>%
      dplyr::mutate(temp_names = paste(labelclass, temp_names, sep = ': ')) %>%
      dplyr::ungroup() %>%
      dplyr::select(temp_names) %>%
      dplyr::transmute(hosts = paste(temp_names, collapse = '; ')) %>%
      dplyr::distinct() %>% as.data.frame() -> testXYZ
      #unlist()
    test_host_fun <- eppo_tabletools_hosts(testing_names, eppo_token)[[2]] %>%
                       dplyr::filter(eppocode == 'LASPPA') %>%
                       dplyr::select(hosts)
  expect_equal(test_host_fun, compact_names_test)

})

#eppo_tabletools_hosts(testing_names, 'some chars')
