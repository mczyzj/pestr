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
  jsonlite::fromJSON(httr::content(httr::GET(urls), "text"))
}
