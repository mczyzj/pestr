#' EPPO table manipulation tools
#'
#' \code{eppo_tabletools_names} creates table with names -- preferred, common
#' and synonyms -- of pests.
#' \code{eppo_tabletools_hosts} creates table with hosts of pests.
#' \code{eppo_tabletools_cat} creates table with categorization of pests.
#' \code{eppo_tabletools_taxo} creates table with taxonomy of pests. All
#' functions return both long tables and compact, human friendly tables
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

  #take long table and colapse all the host names into one string,
  #separeted with names of host categories (major, minor, incidental etc.)
  compact_table <- hosts_table %>%
    group_by(.data$eppocode) %>%
    select('labelclass', 'full_name') %>%
    mutate(hosts = paste(.data$full_name, collapse = ', ')) %>%
    mutate(hosts = paste(.data$labelclass, ': ', .data$hosts, collapse = '; ')) %>%
    ungroup() %>%
    select('eppocode', 'hosts') %>%
    distinct()

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
    #create reusable variables to access EPPO API
    eppocodes <- names_tables[[3]]$eppocode
    cat_urls <-paste0('https://data.eppo.int/api/rest/1.0/taxon/',
                        eppocodes, '/categorization', token)
    #download data on categorization from EPPO and strore them as list of tables
    cat_list_table <- lapply(cat_urls,
                             function(x) jsonlite::fromJSON(RCurl::getURL(x)))
    cat_tables <- setNames(vector("list", length(eppocodes)), eppocodes)
    #exchange empty lists with NA tables -> NEEDS REFACTORING!!!
    for (i in 1:length(cat_list_table)) {
      if (rlang::is_empty(cat_list_table[[i]]) == TRUE) {
        cat_tables[[i]] <- data.frame(nomcontinent = NA,
                                      isocode = NA,
                                      country = NA,
                                      qlist = NA,
                                      qlistlabel = NA,
                                      yr_add = NA,
                                      yr_del = NA,
                                      yr_trans = NA)
      } else {
        cat_tables[[i]] <- cat_list_table[[i]]
      }
    }

    #collapse values in list tables into whole categorization in one cell
    #for each of the pests

    compact_list <- setNames(vector("list", length(eppocodes)), eppocodes)

    for (i in 1: length(cat_tables)) {
      compact_list[[i]] <- cat_tables[[i]] %>%
        tidyr::nest(.data$nomcontinent) %>%
        mutate(categorization = paste0(.data$country, ': ',
                                       .data$qlistlabel, ': ',
                                       'add/del/trans: ',
                                       .data$yr_add, '/',
                                       .data$yr_del, '/',
                                       .data$yr_trans)) %>%
        tidyr::unnest() %>%
        select('nomcontinent', 'categorization') %>%
        group_by(.data$nomcontinent) %>%
        mutate(categorization = paste(.data$categorization,
                                      collapse = '; ')) %>%
        distinct(.data$categorization) %>%
        mutate(categorization = paste(.data$nomcontinent,
                                      .data$categorization,
                                      sep = ': ')) %>%
        ungroup() %>%
        transmute(categorization = paste(.data$categorization,
                                         collapse = ' | ')) %>%
        distinct()
    }

    compact_table <- compact_list %>%
      bind_rows(.id = 'eppocode')

    return(list(list_table = cat_tables,
              compact_table))
}
}
#' @rdname eppo_tabletools
#' @export
eppo_tabletools_taxo <- function(names_tables, token) {
  if (!all(class(token) == c('pestr_token', 'character'))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {
    eppocodes <- names_tables[[3]]$eppocode
    taxo_urls <-paste0('https://data.eppo.int/api/rest/1.0/taxon/',
                        eppocodes, '/taxonomy', token)
    taxo_list_table <- lapply(taxo_urls,
                              function(x) jsonlite::fromJSON(RCurl::getURL(x)))
    #Create empty table and NA table
    empty_taxo_df <- data.frame(codeid = NA,
                                eppocode = NA,
                                prefname = NA,
                                level = NA)
    taxo_tables <- setNames(vector("list", length(eppocodes)), eppocodes)

    #If table from list is empty exchange it with NA table
    for (i in 1:length(taxo_list_table)) {
      if (rlang::is_empty(taxo_list_table[[i]]) == TRUE) {
        taxo_tables[[i]] <- empty_taxo_df
      } else {
        taxo_tables[i] <- taxo_list_table[i]
      }
    }
    #Create compact table
    compact_table <- data.frame(eppocode = eppocodes,
                                taxonomy = rep(NA, length(eppocodes)),
                                stringsAsFactors = FALSE)

    for (i in 1:length(eppocodes)) {
      if(taxo_tables[[i]][1, 3] == 'Animalia') {
        compact_table[i, 2] <- taxo_tables[[i]][2, 3]
      } else if (taxo_tables[[i]][1, 3] == 'Viruses and viroids'){
        compact_table[i, 2] <- taxo_tables[[i]][2, 3]
      } else {
        compact_table[i, 2] <- taxo_tables[[i]][1, 3]
      }
    }

    return(list(list_table = taxo_tables,
                compact_table = compact_table))
  }
}
