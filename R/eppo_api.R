#' EPPO API helper tools
#'
#' Set of small wrappers and functions to help connecting with API and reuse
#' among other package functions
#'
#' @param eppocodes character vector of eppocodes to be downloaded.
#' @param type string, one of: hosts, categorization, taxonomy, pests
#' @param token An object containing EPPO API token created via
#'   {\link{create_eppo_token}}.
#' @return List containing two data frames. First is in a long format, and each
#'   row contains synonyms and names in other languages in respect to preferred
#'   names. The second data frame contains coerced synonyms and other names to
#'   single cell for each preferred name.
#' @noRd

eppo_rest_download <- function(eppocodes, type, token) {
  urls <-paste0('https://data.eppo.int/api/rest/1.0/taxon/',
                      eppocodes, paste0("/", type), token)
  #download data on (...) from EPPO and strore them as list
  hosts_download <- lapply(urls,
                           function(x) jsonlite::fromJSON(RCurl::getURL(x)))
}
