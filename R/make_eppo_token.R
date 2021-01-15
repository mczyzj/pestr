#' EPPO Data Services token variable
#'
#' `r lifecycle::badge("maturing")`
#' \code{create_eppo_token} makes token thats should be saved in a variable and
#' used argument for \code{\link{eppo_tabletools_hosts}},
#' \code{\link{eppo_tabletools_pests}}, \code{\link{eppo_tabletools_cat}} and
#' \code{\link{eppo_tabletools_taxo}} functions. It contains token of class
#' string, that is needed to communicate with EPPO Data Services API.
#'
#' @param x A string.
#' @return Token to access EPPO Data Services API in format recognized by
#'  \code{eppo_tabletools_*} functions.
#' @seealso To obtain your free EPPO token please register
#' \url{https://data.eppo.int/}
#' @examples
#' \dontrun{
#'   create_eppo_token("12345678abcdef")
#' }
#' @export
create_eppo_token <- function(x) {
  eppo_token <- NULL
  character_token <- as.character(x)
  if (grepl('[^a-f0-9]', character_token)) {
    #not sure if this should result in error - to consider
    message(msg_helper("forbidden_chars"))
    return(invisible(NULL))
  } else {
    character_token <- as.character(paste0('?authtoken=', character_token))
    eppo_token <- structure(character_token,
                             class = c('pestr_token', 'character'))
    return(eppo_token)
  }
}

#' EPPO Data Services token variable
#'
#' `r lifecycle::badge("maturing")`
#' \code{check_eppo_token} should be used after \code{create_eppo_token}
#' to check if the token is correctly recognized by EPPO Data Services API.
#' As a reference it uses link to *Xylella fastidiosa* hosts database -
#' **XYLEFA** eppocode.
#' If token is recognized there will be no message. In other cases function will
#' show following messages: No internet connection or Forbidden (HTTP 403).
#'
#' @param token object of class pestr_token.
#' @return Silent NULL when there is no error, otherwise message.
#' @seealso To obtain your free EPPO token please register
#' to **EPPO Data Services** \url{https://data.eppo.int/}
#' @examples
#' \dontrun{
#' eppo_token <- create_eppo_token("abcdef123456")
#' check_eppo_token(eppo_token)
#' }
#' @export
check_eppo_token <- function(token) {
  if (!all(inherits(token, c('pestr_token')))) {
    message(msg_helper("wrong_token"))
  } else {
    # First check internet connection
    if (!curl::has_internet()) {
      message("No internet connection.")
      return(invisible(NULL))
      }
    # Then try for timeout problems
    resp <- try_GET(paste0(
      "https://data.eppo.int/api/rest/1.0/taxon/XYLEFA/hosts", token))
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
