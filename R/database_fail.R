#' EPPO Data Services Database downloads check
#'
#' `r lifecycle::badge("stable")`
#' Fail Gracefully when fail to download eppo database.
#'
#' @param zipfile A string with path to eppocodes.zip file is stored.
#' @param link A string with name of the file. By default it takes name
#'    eppocodes.sqlite.
#' @return NULL
#' @noRd
eppo_database_helper <- function(zipfile = NULL,
                                 link = NULL) {
  if (!isTRUE(eppo_database_check())){
    print(link)
    try_GET <- function(x, ...) {
      tryCatch(
        curl::curl_download(url = link, destfile = zipfile, mode = "wb", ...),
        error = function(e) conditionMessage(e),
        warning = function(w) conditionMessage(w)
      )
    }

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(link)
  if (!inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
  }
}
