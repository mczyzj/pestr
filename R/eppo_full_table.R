#' EPPO Data Services information table
#'
#' `r lifecycle::badge("maturing")`
#' \code{eppo_table_full} creates table in human friendly format with data on
#' pest preferred name, synonyms and common names, hosts, categorization,
#' taxonomy and its distribution.
#'
#' @param names_vector A vector with organism names (or part of the names) to be
#'    checked for existence in EPPO SQLite Database.
#' @param sqlConnection connection to SQLite EPPO Database. By default NULL;
#'    function will automatically connect to database with default credentials.
#' @param token An object containing EPPO API token created via
#'   {\link{create_eppo_token}}.
#' @return Wraps results of function from \code{eppo_tabletools}, namely:
#'    {\link{eppo_tabletools_names}}, {\link{eppo_tabletools_hosts}},
#'    {\link{eppo_tabletools_cat}}, {\link{eppo_tabletools_taxo}}, and
#'    {\link{eppo_tabletools_distri}} into one compact, human readable table.
#' @export
eppo_table_full <- function(names_vector, sqlConnection, token) {
  if (!all(class(token) == c('pestr_token', 'character'))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {

  names_tables <- eppo_names_tables(names_vector, sqlConnection)
  #create intermediate table for names, hosts, categorization, taxonomy
  #and distribution
  compact_names  <- eppo_tabletools_names(names_tables)
  compact_hosts  <- eppo_tabletools_hosts(names_tables, token)
  compact_cat    <- eppo_tabletools_cat(names_tables, token)
  compact_taxo   <- eppo_tabletools_taxo(names_tables, token)
  compact_distri <- eppo_tabletools_distri(names_tables)
  #full join all the intermediate compact tables into one table
  #each row is one pest
  full_table  <-  compact_names[[2]] %>%
    dplyr::full_join(compact_hosts[[2]], by = 'eppocode') %>%
    dplyr::full_join(compact_cat[[2]], by = 'eppocode') %>%
    dplyr::full_join(compact_distri[[2]], by = 'eppocode') %>%
    dplyr::full_join(compact_taxo[[2]], by = 'eppocode')

  return(full_table)
  }
}
