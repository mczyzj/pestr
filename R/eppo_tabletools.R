#' EPPO table manipulation tools
#'
#' \code{eppo_tabletools_names} creates table with names.
#'
#' @param names_tables A list of tables created via {\link{eppo_names_tables}}.
#' @return List containing two data frames. First is in a long format, and each
#'   row contains synonyms and names in other languages in respect to preferred
#'   names. The second data frame contains coerced synonyms and other names to
#'   single cell for each preferred name.
#' @name eppo_tabletools
NULL

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_names <- function(names_tables) {
  other_table <- names_tables$all_associated_names %>%
    filter(.data$preferred == 0) %>%
    rename(Other_names = .data$fullname) %>%
    select('codeid'       = 1,
           'Other_names'  = 2,
           'codelang'     = 4)

  preferred_table <- names_tables$all_associated_names %>%
    filter(.data$preferred == 1) %>%
    rename(Preferred_name = .data$fullname) %>%
    select('codeid', 'eppocode', 'Preferred_name') %>%
    left_join(other_table, by = 'codeid') %>%
    mutate(Name_type = ifelse(.data$codelang == 'la',
                              'Synonym', 'Other languages')) %>%
    mutate(Other_names = replace(.data$Other_names,
                                 is.na(.data$Other_names), 'none'),
           codelang    = replace(.data$codelang,
                                 is.na(.data$codelang), 'none'),
           Name_type   = replace(.data$Name_type,
                                 is.na(.data$Name_type), 'Preferred')) %>%
    arrange(.data$Preferred_name, desc(.data$Name_type), .data$Other_names)

  return(list(long_table = preferred_table,
              compact_table = data.frame()))
}
