context("EPPO tabletools functions")
library("pestr")
library("dplyr")
library("rlang")

##### EPPO TABLETOOLS NAMES #####
test_that("Test that names f returns correct structure from database", {
  testing_names <- eppo_names_tables('Xylella')
  result_names <- eppo_tabletools_names(testing_names)

  expect_is(result_names, 'list')
  expect_is(result_names[[1]], 'data.frame')
  expect_is(result_names[[2]], 'data.frame')
})

test_that("Test that names f creates 0 row data frame when there are no
          preffered names", {
  testing_names <- eppo_names_tables('abcdefghij')
  result_names <- eppo_tabletools_names(testing_names)
  expect_equal(dim(result_names$long_table)[1], 0)
  expect_equal(dim(result_names[[2]])[1], 0)
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
  expect_equal(result_names[[2]]$Other_names[1], cydia_test$Other_names)
})

##### EPPO TABLETOOLS HOSTS ####
test_that("Test that hosts f checks if parsed arguments are of proper class", {
  testing_names <- eppo_names_tables('Xylella')
  eppo_token <- create_eppo_token('abc123')
  expect_message(eppo_tabletools_hosts(testing_names, 'some chars'),
                 'Please provide token created with create_eppo_token function')
})

 test_that("Test that hosts f returns correct structure from database", {
  testing_names <- eppo_names_tables('Xylella')
  eppo_token <- create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token

  tester_host_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, hosts, token) readRDS("mocked_hosts_xylella.RDS"),
      eppo_tabletools_hosts(testing_names, eppo_token)
    )
  }

  test_hosts <- tester_host_func()

  expect_is(test_hosts, 'list')
  expect_is(test_hosts[[1]], 'data.frame')
  expect_is(test_hosts[[2]], 'data.frame')
})

test_that("Test that hosts f works correctly", {
  skip_on_travis()
  skip_on_cran()
 # skip('Only for use locally with proper token.') #comment out to test
  testing_names <- eppo_names_tables(c('Cydia packardi', 'Tuta absoluta',
                                       'Abies alba'))
  eppo_token <- create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token
  testing_hosts <- readRDS("mocked_hosts.RDS")
  eppocode <- testing_names[[3]]$eppocode
  names(testing_hosts) <- eppocode

  for(i in 1:length(testing_hosts))   {
    if(rlang::is_empty(testing_hosts[[i]])) {
      testing_hosts[[i]] <- list(Host = data.frame(
        codeid = NA,
        eppocode = NA,
        idclass = 9,
        labelclass = "Host",
        full_name = NA
      ))
    }
  }
  test_host_table <- lapply(testing_hosts, dplyr::bind_rows) %>%
    dplyr::bind_rows(.id = 'pest_code') %>%
    dplyr::rename(host_eppocode = eppocode, eppocode = pest_code)

  tester_host_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, hosts, token) readRDS("mocked_hosts.RDS"),
      eppo_tabletools_hosts(testing_names, eppo_token)
    )
  }

  test_hosts <- tester_host_func()

  expect_equal(test_hosts[[1]],
               test_host_table)

  ###test compact table values

  compact_names_test <- test_host_table %>%
    dplyr::select(.data$eppocode, .data$labelclass, .data$full_name) %>%
    dplyr::group_by(.data$eppocode, .data$labelclass) %>%
    dplyr::mutate(hosts = paste(.data$full_name, collapse = ', ')) %>%
    dplyr::mutate(hosts = paste0(.data$labelclass, ': ', .data$hosts)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$eppocode, .data$hosts) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$eppocode) %>%
    dplyr::mutate(hosts = paste(.data$hosts, collapse = '; ')) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  expect_equal(test_hosts[[2]], compact_names_test)

})

##### EPPO TABLETOOLS CAEGORIZATION ####

test_that("Test that categorization f checks if parsed arguments
          are of proper class", {
  testing_names <- eppo_names_tables('Xylella')
  eppo_token <- create_eppo_token('abc123')
  expect_message(eppo_tabletools_cat(testing_names, 'some chars'),
                 'Please provide token created with create_eppo_token function')
})

test_that("Test that categorization f returns correct structure
          from database", {
  testing_names <- eppo_names_tables('Xylella')
  eppo_token <- create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token

  tester_cat_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, categorization, token) readRDS("mocked_cat_xylella.RDS"),
      eppo_tabletools_cat(testing_names, eppo_token)
    )
  }

  test_cat <- tester_cat_func()

  expect_is(test_cat, 'list')
  expect_is(test_cat[[1]], 'data.frame')
  expect_is(test_cat[[2]], 'data.frame')
})

test_that("Test that cat f works correctly", {
  testing_names <- eppo_names_tables(c('Cydia packardi', 'Tuta absoluta',
                                       'Abies alba'))
  eppo_token <- create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token
  eppocodes <- testing_names[[3]]$eppocode
  testing_cat <- readRDS("mocked_cat.RDS")
  names(testing_cat) <- eppocodes
  transformed_cat <- setNames(vector("list", length(eppocodes)), eppocodes)
  #exchange empty lists with NA tables
  for (i in 1:length(testing_cat)) {
    if (rlang::is_empty(testing_cat[[i]]) == TRUE) {
      transformed_cat[[i]] <- data.frame(nomcontinent = NA,
                                         isocode = NA,
                                         country = NA,
                                         qlist = NA,
                                         qlistlabel = NA,
                                         yr_add = NA,
                                         yr_del = NA,
                                         yr_trans = NA)
    } else {
      transformed_cat[[i]] <- testing_cat[[i]]
    }
  }

  transformed_cat_lt <- transformed_cat %>%
    dplyr::bind_rows(.id = "eppocode")

  tester_cat_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, categorization, token) readRDS("mocked_cat.RDS"),
      eppo_tabletools_cat(testing_names, eppo_token)
    )
  }

  test_cat <- tester_cat_func()

  expect_equal(test_cat[[1]] %>% dplyr::filter(.data$eppocode == "LASPPA"),
               transformed_cat_lt %>% dplyr::filter(.data$eppocode == "LASPPA"))
  expect_equal(test_cat$long_table %>% dplyr::filter(.data$eppocode == "ABIAL"),
               transformed_cat_lt %>% dplyr::filter(.data$eppocode == "ABIAL"))

  #test compact table values

  compact_list <- setNames(vector("list", length(eppocodes)), eppocodes)

  for (i in 1: length(transformed_cat)) {
    compact_list[[i]] <- transformed_cat[[i]] %>%
      tidyr::nest(data = .data$nomcontinent) %>%
      dplyr::mutate(categorization = paste0(country, ': ', qlistlabel, ': ',
                                            'add/del/trans: ',
                                            yr_add, '/', yr_del, '/', yr_trans)) %>%
      tidyr::unnest(cols = .data$data) %>%
      dplyr::select(.data$nomcontinent, .data$categorization) %>%
      dplyr::group_by(.data$nomcontinent) %>%
      dplyr::mutate(categorization = paste(categorization, collapse = '; ')) %>%
      dplyr::distinct(categorization) %>%
      dplyr::mutate(categorization = paste(.data$nomcontinent, .data$categorization, sep = ': ')) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(categorization = paste(categorization, collapse = ' | ')) %>%
      dplyr::distinct()
  }

  compact_table <- bind_rows(compact_list, .id = 'eppocode')

  expect_equal(test_cat[[2]],
               compact_table)
})

##### EPPO TABLETOOLS TAXONOMY ####

test_that("Test that taxonomy f checks if parsed arguments
          are of proper class", {
  testing_names <- eppo_names_tables('Xylella')
  eppo_token <- create_eppo_token('abc123')
  expect_message(eppo_tabletools_taxo(testing_names, 'some chars'),
                           'Please provide token created with create_eppo_token function')
})

test_that("Test that taxonomy f returns correct structure
          from database", {
  testing_names <- eppo_names_tables(c('Cydia packardi', 'cadang',
                                     'Plasmodiophora brassicae', 'Abies alba',
                                     'Pantoea stewartii', 'Globodera pallida',
                                     'Phialophora cinerescens'))
  eppo_token <- create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token

  tester_taxo_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, categorization, token) readRDS("mocked_taxo.RDS"),
      eppo_tabletools_taxo(testing_names, eppo_token)
    )
  }

  test_taxo <- tester_taxo_func()

  expect_is(test_taxo, 'list')
  expect_is(test_taxo[[1]], 'data.frame')
  expect_is(test_taxo[[2]], 'data.frame')
})

test_that("Test that taxonomy f works correctly", {
  testing_names <- eppo_names_tables(c('Cydia packardi', 'cadang',
                                       'Plasmodiophora brassicae', 'Abies alba',
                                       'Pantoea stewartii', 'Globodera pallida',
                                       'Phialophora cinerescens'))

  eppo_token <- create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token

  eppocodes <- testing_names[[3]]$eppocode
  testing_taxo <- readRDS("mocked_taxo.RDS")
  names(testing_taxo) <- eppocodes

  test_taxon_names <- data.frame(eppocode = c("HETDPA", "LASPPA", "PHIACI",
                                              "PLADBR", "ABIAL", "CCCVD0",
                                              "ERWIST", "PNTOIN"),
                                 taxonomy = c("Nematoda", "Arthropoda", "Fungi",
                                              "Protista", "Plantae", "Riboviria",
                                              "Bacteria", "Bacteria"),
                                 stringsAsFactors = FALSE)

  tester_taxo_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, categorization, token) readRDS("mocked_taxo.RDS"),
      eppo_tabletools_taxo(testing_names, eppo_token)
    )
  }

  test_taxo <- tester_taxo_func()

  expect_equal(test_taxo[[1]][1, 3], "Animalia")
  expect_equal(test_taxo[[1]][15, 3], "Grapholita packardi")
  expect_equal(test_taxo[[1]][25, 1], 60625)
  expect_equal(test_taxo[[1]][39, 2], "CCCVD0")

  expect_equal(test_taxo[[2]], test_taxon_names)

})

##### EPPO TABLETOOLS DISTRIBUTION ####

test_that("Test that distribution f returns correct structure
          from database", {

  testing_names <- eppo_names_tables(c('Cydia packardi', 'cadang',
                                       'Plasmodiophora brassicae', 'Abies alba',
                                       'Pantoea stewartii', 'Globodera pallida',
                                       'Phialophora cinerescens'))

  tester_distri_func <- function() {
    mockr::with_mock(
      eppo_csv_download = function(eppocodes) readRDS("mocked_distri.RDS"),
      eppo_tabletools_distri(testing_names)
    )
  }

 test_distri <- tester_distri_func()


 expect_is(test_distri, 'list')
expect_is(test_distri[[1]], 'data.frame')
 expect_is(test_distri[[2]], 'data.frame')
})

test_that("Test that distribution f returns correct values
          from database", {

  testing_names <- eppo_names_tables(c('Cydia packardi', 'cadang',
                                       'Plasmodiophora brassicae', 'Abies alba',
                                       'Pantoea stewartii', 'Globodera pallida',
                                       'Phialophora cinerescens'))
  eppocodes <- testing_names[[3]]$eppocode

  distri_lists <- readRDS("mocked_distri.RDS")

  tester_distri_func <- function() {
    mockr::with_mock(
      eppo_csv_download = function(eppocodes) readRDS("mocked_distri.RDS"),
      eppo_tabletools_distri(testing_names)
    )
  }

  test_distri <- tester_distri_func()

  distri_lists <- readRDS("mocked_distri.RDS")

  for (i in 1:length(distri_lists)) {
    if (dim(distri_lists[[i]])[1] == 0) {
      distri_lists[[i]] <- data.frame(continent    = NA,
                                      country      = NA,
                                      state        = NA,
                                      country.code = NA,
                                      state.code   = NA,
                                      Status       = NA)
    }
  }

  distri_df <- distri_lists %>%
    dplyr::bind_rows(.id = 'eppocode')

  testing_distri_df <- distri_lists %>%
    dplyr::bind_rows(.id = 'eppocode') %>%
    dplyr::filter(!grepl("Absent", Status)) %>%
    dplyr::select('eppocode', 'continent', 'country') %>%
    dplyr::group_by(eppocode, continent) %>%
    dplyr::distinct() %>%
    dplyr::mutate(distribution = paste(country, collapse = ', ')) %>%
    dplyr::mutate(distribution = paste(continent, distribution, sep = ': ')) %>%
    dplyr::ungroup() %>%
    dplyr::select('eppocode', 'distribution') %>%
    dplyr::distinct() %>%
    dplyr::group_by(eppocode) %>%
    dplyr::mutate(distribution = paste(distribution, collapse = '; ')) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  expect_equal(test_distri[[1]], distri_df)
  expect_equal(test_distri[[2]], testing_distri_df)
})

##### EPPO TABLETOOLS PESTS ####

test_that("Test that pests f returns correct structure
          from database", {
  testing_names <- eppo_names_tables(c('Triticum aestivum', 'Abies alba'))

  eppo_token <- create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token

  tester_pest_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, pests, token) readRDS("mocked_pests.RDS"),
      eppo_tabletools_pests(testing_names, eppo_token)
    )
  }

  test_pests <- tester_pest_func()

  expect_is(test_pests, 'list')
  expect_is(test_pests[[1]], 'data.frame')
  expect_is(test_pests[[2]], 'data.frame')
})

test_that("Test that pest f works correctly", {
  eppo_token <-create_eppo_token('e3ecef2dea564abec28e1181eb3b1b11') #artificial token

  testing_names <- eppo_names_tables(c('Triticum aestivum', 'Abies alba'))

  tester_pest_func <- function() {
    mockr::with_mock(
      eppo_rest_download = function(eppocodes, pests, token) readRDS("mocked_pests.RDS"),
      eppo_tabletools_pests(testing_names, eppo_token)
    )
  }

  test_pests <- tester_pest_func()

  expect_equal(test_pests[[2]]$eppocode, c("ABIAL", "TRZAC", "TRZAX", "TTLRI"))
  expect_true(grepl("Listronotus bonariensis",
                    test_pests[[2]]$pests[3]))
  expect_false(grepl("Listronotusbonariensis",
                     test_pests[[2]]$pests[3]))
  expect_true(all(test_pests[[1]]$labelclass %in% c("Incidental",
                                                    "Host",
                                                    "Major host",
                                                    "Minor host",
                                                    "Unclassified",
                                                    "Artificial",
                                                    "Experimental")))
})
