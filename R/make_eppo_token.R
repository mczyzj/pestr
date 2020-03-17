#' EPPO Data Services token variable
#'
#' \code{create_eppo_token} makes global variable eppo_token. It contains token
#' of class string, that is needed to communicate with EPPO Data Services API.
#' Global variable is shared between package functions.
#'
#' @param x A string.
#' @return Global variable with the token to access EPPO Data Services API.
#' @seealso To obtain your free EPPO token please register
#' \url{https://data.eppo.int/}
#' @examples
#' \dontrun{
#' create_eppo_token("12345678abcdef")
#' }
#' @export
create_eppo_token <- function(x) {
  eppo_token <- NULL
  character_token <- as.character(x)
  if (grepl('[^a-f0-9]', character_token)) {
    warning('Token contains forbiden characters')
  } else {
    character_token <- as.character(paste0('?authtoken=', character_token))
    eppo_token <<- structure(character_token,
                             class = c('pestr_token', 'character'))
  }
}
