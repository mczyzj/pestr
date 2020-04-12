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
