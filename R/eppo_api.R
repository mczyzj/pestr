#' Fail Gracefuly API helper tools
#'
#' Try-catch wrapper.
#'
#' @param x character vector of urls including token, to be downloaded.
#'
#' @return List of REST download results.
#' @noRd

try_GET <- function(x, ...) {
  tryCatch(
    httr::GET(url = x, httr::timeout(20), ...),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
}

#' Fail Gracefuly API helper tools
#'
#' Small wrapper for graceful fail if there is an error with internet
#' connection, wrong token or wrong link.
#' The solution is based on
#' https://bit.ly/2WYD1cC as suggested by kvasilopoulos
#'
#' @param urls character vector of urls including token, to be downloaded.
#'
#' @return List of REST download results.
#' @noRd

eppo_try_urls <- function(urls) {

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

    # Then try for timeout problems
  resp <- try_GET(urls)
  if (!inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }

  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    message("\nMost likely one of the provided EPPO codes or token is invalid.")
  }

  return(resp)
}

#' EPPO API helper tools
#'
#' Set of small wrappers and functions to help connecting with API and reuse
#' among other package functions
#'
#' @param eppocodes character vector of eppocodes to be downloaded.
#' @param type string, one of: hosts, categorization, taxonomy, pests
#' @param token An object containing EPPO API token created via
#'   {\link{create_eppo_token}}.
#' @return List of REST download results.
#' @noRd

eppo_rest_download <- function(eppocodes, type, token) {
  urls <-paste0('https://data.eppo.int/api/rest/1.0/taxon/',
                      eppocodes, paste0("/", type), token)
  lapply(urls, eppo_json_wrapper)
}

#' JSON wrapper
#'
#' Set of small wrappers and functions to help connecting with API and reuse
#' among other package functions
#'
#' @param urls character vector of eppocodes to be downloaded.
#' @return List of REST download results.
#' @noRd

eppo_json_wrapper <- function(urls) {
  querry_content <- jsonlite::fromJSON(httr::content(eppo_try_urls(urls), "text"))

  if (is.null(querry_content)) {
    querry_content <- list()
  } else if (length(querry_content) == 1 &
             "message" %in% names(querry_content)) {
    querry_content <- list()
  }

  return(querry_content)
}

#' EPPO distribution helper tools
#'
#' Set of small wrappers and functions to help connecting with API and reuse
#' among other package functions
#'
#' @param eppocodes character vector of eppocodes to be downloaded.
#' @return List of CSV download results.
#' @noRd

eppo_csv_download <- function(eppocodes) {
  distri_urls <- paste0('https://gd.eppo.int/taxon/',
                        eppocodes,'/download/distribution_csv')

  distri_lists <- stats::setNames(vector("list", length(eppocodes)), eppocodes)

  # download csv files directly into list
  for (i in 1:length(distri_lists)) {
    distri_lists[[i]] <- eppo_try_urls(distri_urls[i]) %>%
      httr::content(type = "text/csv",
                    encoding = "UTF-8",
                    col_types = readr::cols()) %>%
      as.data.frame()
  }
  #If the eppo code was not recognized the file will be empty, with no correct
  #column names. Delete by substituting those wrong elements of the list
  #with NULL and print the wrong codes.
  for (i in names(distri_lists)) {
    if (!all(names(distri_lists[[i]]) %in%
             c("continent", "country", "state",
               "country code", "state code", "Status"))) {
      message(msg_helper("no_distri", i))
      distri_lists[[i]] <- NULL
    } else {
      colnames(distri_lists[[i]]) <- c("continent", "country", "state",
                                       "country.code", "state.code", "Status")
    }
  }
  return(distri_lists)
}

#' EPPO check eppocodes helper tools
#'
#' Set of small wrappers and functions to help connecting with API and reuse
#' among other package functions
#'
#' @param eppocodes character vector of eppocodes to be downloaded.
#' @return List of CSV download results.
#' @noRd
check_eppocodes <- function(eppocodes) {


  grep("[A-Z]{1,}", grep("^[A-Z0-9]{5,6}$", eppocodes, value = TRUE), value = TRUE)

  correct_str <- grep("[A-Z]{1,}",
                      grep("^[A-Z0-9]{5,6}$", eppocodes, value = TRUE),
                      value = TRUE)
  incorrect_str <- eppocodes[!(eppocodes %in% correct_str)]

  if (length(incorrect_str) != 0) {
    message(msg_helper("wrong_eppocodes", incorrect_str))
  }

  if (length(correct_str) == 0) {
    return(NULL)
  }

  return(correct_str)
}

#' EPPO change empty eppocodes to NULL
#'
#' Set of small wrappers and functions to help connecting with API and reuse
#' among other package functions
#'
#' @param eppocodes character vector of eppocodes to be downloaded.
#' @return List of CSV download results.
#' @noRd

null_eppocodes <- function(eppocodes) {

  if(rlang::is_empty(eppocodes)) {
    return(NULL)
  } else {
    return(eppocodes)
  }

}
