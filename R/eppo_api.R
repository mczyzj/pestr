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
    httr::GET(url = x, httr::timeout(15), ...),
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

#  jsonlite::fromJSON(httr::content(httr::GET(urls), "text"))
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

  for (i in 1:length(distri_lists)) {
    distri_lists[i][[1]] <- utils::read.csv(file = distri_urls[i],
                                            header = T, stringsAsFactors = F)
  }
  return(distri_lists)
}
