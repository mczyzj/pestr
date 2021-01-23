#' Citation helper tools
#'
#' `r lifecycle::badge("experimental")`
#' Citation returns citation of EPPO Data Services and/or EPPO Global Database
#' in bibtex format. To save the output into file use \code{output} argument
#' with destination file name.
#'
#' @param cite string. Possible values: 'data_services', 'global_database' or
#'  'both'.
#' @param output string. Name of the file to which output will be saved. If NULL
#'  (default) results will be printed to console.
#'
#' @return citation of EPPO Data Services, EPPO Global Database or both in
#'   bibtex format.
#' @examples
#' eppo_citation("data_services")
#' eppo_citation("global_database")
#' eppo_citation("both")
#' @export


eppo_citation <- function(cite, output = NULL) {

  if (length(cite) != 1) {
    message(msg_helper("cite_length"))
    return(invisible(NULL))
  }

  if (!(cite %in% c("data_services", "global_database", "both"))) {
    message(msg_helper("cite_arg", cite))
    return(invisible(NULL))
  }

  if (!is.null(output) & !inherits(output, "character")) {
    message(msg_helper("wrong_output"))
    return(invisible(NULL))
  }

  if (!is.null(output)) {
    sink(output)
  }

  eppo_gd_cit <- "@online{eppoGD,
    title = {EPPO Global Database},
    author = {{EPPO}},
    organization = {European and Mediterranean Plant Protection Organization},
    address = {Paris, France},
    year = {2021},
    url = {https://gd.eppo.int/},
    urldate = {},
  }"

  eppo_ds_cit <- "@online{eppoDS,
    title = {EPPO Data Services},
    author = {{EPPO}},
    organization = {European and Mediterranean Plant Protection Organization},
    address = {Paris, France},
    year = {2021},
    url = {https://data.eppo.int/},
    urldate = {},
  }"

  switch(
    cite,
    "global_database" = cat(eppo_gd_cit),
    "data_services" = cat(eppo_ds_cit),
    "both" = cat(eppo_ds_cit, "\n\n", eppo_gd_cit, sep ="")
  )

  message(msg_helper("cite_remember"))

  if (!is.null(output)) {
    sink()
    message(msg_helper("cite_output", output))
  }
}
