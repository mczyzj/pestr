#' EPPO table manipulation tools
#'
#' \code{eppo_tabletools_names} creates table with names.
#'
#' @param names_tables A list of tables created via {\link{eppo_names_tables}}.
#' @param token An object containing EPPO API token created via
#'   {\link{create_eppo_token}}.
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

  temp_table <- preferred_table %>%
    select('codeid', 'eppocode',
           'Preferred_name', 'Other_names', 'Name_type') %>%
    tidyr::nest(.data$Name_type, .data$Other_names)

  temp_Index <- seq(length(temp_table$data))

  for (i in 1:length(temp_table$data)) {
    temp_table$data[[i]] %>%
      arrange(desc(.data$Name_type), .data$Other_names) %>%
      group_by(.data$Name_type) %>%
      mutate(temp_names = paste(.data$Other_names, collapse = ', ')) %>%
      distinct(.data$temp_names) %>%
      mutate(temp_names = paste(.data$Name_type, .data$temp_names,
                                sep = ': ')) %>%
      ungroup() %>%
      select(.data$temp_names) %>%
      transmute(alt_names = paste(.data$temp_names[1], .data$temp_names[2],
                                  sep = '; ')) %>%
      distinct() %>%
      gsub('; NA', '', .) -> temp_Index[i]
  }

  compact_table <- temp_table %>%
    mutate(data = temp_Index) %>%
    rename(Other_names = .data$data)

  return(list(long_table = preferred_table,
              compact_table))
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_hosts <- function(names_tables, token) {
  if (!all(class(token) == c('pestr_token', 'character'))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {
  #create reusable variables to access EPPO API
  eppocodes <- names_tables[[3]]$eppocode
  hosts_urls <-paste0('https://data.eppo.int/api/rest/1.0/taxon/',
                    eppocodes, '/hosts', token)
  #download data on hosts from EPPO and strore them as list, name each list
  #element with eppocode and bind sub-tables by rows to store them as long table
  hosts_download <- lapply(hosts_urls,
                           function(x) jsonlite::fromJSON(RCurl::getURL(x)))
  names(hosts_download) <- eppocodes
  hosts_table <- lapply(hosts_download, function(x) dplyr::bind_rows(x)) %>%
    bind_rows(.id = 'pest_code') %>%
    rename(host_eppocode = .data$eppocode, eppocode = .data$pest_code)
  #use data from long table to create compact table with all host in one cell
  #per each pest
  nested_hosts <- hosts_table %>%
    select(.data$labelclass, .data$full_name, .data$eppocode) %>%
    tidyr::nest(.data$labelclass, .data$full_name)

  hostIndex <- setNames(vector("list", length(eppocodes)), eppocodes)

  #names(hostIndex) <- eppocodes
  #
  for (i in 1:length(nested_hosts$data)) {
    nested_hosts$data[[i]] %>%
      group_by(.data$labelclass) %>%
      mutate(temp_names = paste(.data$full_name,
                                collapse = ', ')) %>%
      distinct(.data$temp_names) %>%
      mutate(temp_names = paste(.data$labelclass,
                                .data$temp_names,
                                sep = ': ')) %>%
      ungroup() %>%
      select(.data$temp_names) %>%
      transmute(hosts = paste(.data$temp_names, collapse = '; ')) %>%
      distinct() -> hostIndex[i]
  }

  compact_table <- lapply(hostIndex,
         function(hosts) data.frame(hosts, stringsAsFactors = FALSE)) %>%
    bind_rows(.id = 'eppocode')

  return(list(long_table = hosts_table,
                compact_table))
}
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_cat <- function(names_tables, token) {
  if (!all(class(token) == c('pestr_token', 'character'))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {
    cat_table <- data.frame()
    compact_table <- data.frame()
    return(list(long_table = cat_table,
              compact_table))
}
}
