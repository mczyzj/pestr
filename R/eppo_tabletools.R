#' EPPO table manipulation tools
#'
#' \code{eppo_tabletools_names} creates table with names -- preferred, common
#' and synonyms -- of pests. It is particularly useful for creating whole table
#' with {\link{eppo_table_full}}, otherwise it might be more informative to use
#' {\link{eppo_names_tables}} which provides additional information for your
#' query.
#' \code{eppo_tabletools_hosts} creates table with hosts of pests.
#' \code{eppo_tabletools_cat} creates table with categorization of pests.
#' \code{eppo_tabletools_taxo} creates table with taxonomy of pests.
#' \code{eppo_tabletools_distri} creates table with distribution of pests --
#' continent and country level only.
#' \code{eppo_tabletools_pests} creates table with pests of host.
#' All functions return both long tables and compact, human friendly tables.
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
  #intermediate table holding non preffered names with corrected headings
  other_table <- names_tables$all_associated_names %>%
    dplyr::filter(.data$preferred == 0) %>%
    dplyr::rename(Other_names = .data$fullname) %>%
    dplyr::select('codeid'       = 1,
           'Other_names'  = 2,
           'codelang'     = 4)
  #long format table containing collumns for preffered names and accompanying
  #other names, also intermediate table that is a basis for a compact table
  preferred_table <- names_tables$all_associated_names %>%
    dplyr::filter(.data$preferred == 1) %>%
    dplyr::rename(Preferred_name = .data$fullname) %>%
    dplyr::select('codeid', 'eppocode', 'Preferred_name') %>%
    dplyr::left_join(other_table, by = 'codeid') %>%
    dplyr::mutate(Name_type = ifelse(.data$codelang == 'la',
                                     'Synonym', 'Other languages')) %>%
    dplyr::mutate(Other_names = replace(.data$Other_names,
                                        is.na(.data$Other_names), 'none'),
                  codelang    = replace(.data$codelang,
                                        is.na(.data$codelang), 'none'),
                  Name_type   = replace(.data$Name_type,
                                        is.na(.data$Name_type),
                                        'Preferred')) %>%
    dplyr::arrange(.data$Preferred_name, desc(.data$Name_type),
                   .data$Other_names)

  #temporary table nested by name type and other names, in next step
  #names will be collapsed to one cell per preffered name
  temp_table <- preferred_table %>%
    dplyr::select('codeid', 'eppocode',
                  'Preferred_name', 'Other_names', 'Name_type') %>%
    tidyr::nest(.data$Name_type, .data$Other_names)

  temp_Index <- seq(length(temp_table$data))

  for (i in 1:length(temp_table$data)) {
    temp_table$data[[i]] %>%
      dplyr::arrange(desc(.data$Name_type), .data$Other_names) %>%
      dplyr::group_by(.data$Name_type) %>%
      dplyr::mutate(temp_names = paste(.data$Other_names, collapse = ', ')) %>%
      dplyr::distinct(.data$temp_names) %>%
      dplyr::mutate(temp_names = paste(.data$Name_type, .data$temp_names,
                                       sep = ': ')) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$temp_names) %>%
      dplyr::transmute(alt_names = paste(.data$temp_names[1],
                                         .data$temp_names[2], sep = '; ')) %>%
      dplyr::distinct() %>%
      gsub('; NA', '', .) -> temp_Index[i]
  }

  compact_table <- temp_table %>%
    dplyr::mutate(data = temp_Index) %>%
    dplyr::rename(Other_names = .data$data)

  return(list(long_table = preferred_table,
              compact_table))
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_hosts <- function(names_tables, token) {
  if (!all(inherits(token, c('pestr_token')))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {
  #create reusable variables to access EPPO API
  eppocodes <- names_tables[[3]]$eppocode
  #download data on hosts from EPPO and strore them as list, name each list
  #element with eppocode and bind sub-tables by rows to store them as long table
  hosts_download <- eppo_rest_download(eppocodes, "hosts", token)
  #exchange empty lists with NA table to avoid empty host table
    if(is.null(unlist(hosts_download))) {
       hosts_table <- data.frame(eppocode      = eppocodes,
                                 codeid        = names_tables[[3]]$codeid,
                                 host_eppocode = NA,
                                 idclass       = NA,
                                 labelclass    = NA,
                                 full_name     = NA)
    } else {
      names(hosts_download) <- eppocodes
      hosts_table <- lapply(hosts_download, function(x) dplyr::bind_rows(x)) %>%
        dplyr::bind_rows(.id = 'pest_code') %>%
        dplyr::rename(host_eppocode = .data$eppocode,
                      eppocode = .data$pest_code)
    }
  #take long table and colapse all the host names into one string,
  #separeted with names of host categories (major, minor, incidental etc.)
  compact_table <- hosts_table %>%
    dplyr::group_by(.data$eppocode, .data$labelclass) %>%
    dplyr::select('eppocode', 'labelclass', 'full_name') %>%
    dplyr::mutate(hosts = paste(.data$full_name, collapse = ', ')) %>%
    dplyr::mutate(hosts = paste0(.data$labelclass, ': ', .data$hosts)) %>%
    dplyr::ungroup() %>%
    dplyr::select('eppocode', 'hosts') %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$eppocode) %>%
    dplyr::mutate(hosts = paste(.data$hosts, collapse = '; ')) %>%
    dplyr::distinct()

  return(list(long_table = hosts_table,
                compact_table))
}
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_cat <- function(names_tables, token) {
  if (!all(inherits(token, c('pestr_token')))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {
    #create reusable variables to access EPPO API
    eppocodes <- names_tables[[3]]$eppocode
    #download data on categorization from EPPO and strore them as list of tables
    cat_list_table <- eppo_rest_download(eppocodes, "categorization", token)
    cat_tables <- setNames(vector("list", length(eppocodes)), eppocodes)
    #exchange empty lists with NA tables -> NEEDS REFACTORING!!!
    for (i in 1:length(cat_list_table)) {
      if (rlang::is_empty(cat_list_table[[i]]) == TRUE) {
        cat_tables[[i]] <- data.frame(nomcontinent = NA,
                                      isocode      = NA,
                                      country      = NA,
                                      qlist        = NA,
                                      qlistlabel   = NA,
                                      yr_add       = NA,
                                      yr_del       = NA,
                                      yr_trans     = NA)
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
        dplyr::mutate(categorization = paste0(.data$country, ': ',
                                              .data$qlistlabel, ': ',
                                              'add/del/trans: ',
                                              .data$yr_add, '/',
                                              .data$yr_del, '/',
                                              .data$yr_trans)) %>%
        tidyr::unnest() %>%
        dplyr::select('nomcontinent', 'categorization') %>%
        dplyr::group_by(.data$nomcontinent) %>%
        dplyr::mutate(categorization = paste(.data$categorization,
                                             collapse = '; ')) %>%
        dplyr::distinct(.data$categorization) %>%
        dplyr::mutate(categorization = paste(.data$nomcontinent,
                                             .data$categorization,
                                             sep = ': ')) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(categorization = paste(.data$categorization,
                                                collapse = ' | ')) %>%
        dplyr::distinct()
    }

    compact_table <- compact_list %>%
      dplyr::bind_rows(.id = 'eppocode')

    return(list(list_table = cat_tables,
              compact_table))
}
}
#' @rdname eppo_tabletools
#' @export
eppo_tabletools_taxo <- function(names_tables, token) {
  if (!all(inherits(token, c('pestr_token')))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {
    eppocodes <- names_tables[[3]]$eppocode
    taxo_list_table <- eppo_rest_download(eppocodes, "taxonomy", token)
    #Create empty table and NA table
    empty_taxo_df <- data.frame(codeid = NA,
                                eppocode = NA,
                                prefname = NA,
                                level = NA)
    taxo_tables <- stats::setNames(vector("list", length(eppocodes)), eppocodes)

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

    for (j in 1:length(taxo_tables)) {
      if(taxo_tables[[j]][1, 3] == 'Animalia') {
        compact_table[j, 2] <- taxo_tables[[j]][2, 3]
      } else if (taxo_tables[[j]][1, 3] == 'Viruses and viroids'){
        compact_table[j, 2] <- taxo_tables[[j]][2, 3]
      } else {
        compact_table[j, 2] <- taxo_tables[[j]][1, 3]
      }
    }

    return(list(list_table = taxo_tables,
                compact_table = compact_table))
  }
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_distri <- function(names_tables) {
  eppocodes <- names_tables[[3]]$eppocode
  distri_urls <- paste0('https://gd.eppo.int/taxon/',
                       eppocodes,'/download/distribution_csv')

  distri_lists <- stats::setNames(vector("list", length(eppocodes)), eppocodes)
  for (i in 1:length(distri_lists)) {
    distri_lists[i][[1]] <- utils::read.csv(file = distri_urls[i],
                                            header = T, stringsAsFactors = F)
  }

  compact_table <- distri_lists %>%
    dplyr::bind_rows(.id = 'eppocode') %>%
    dplyr::filter(!grepl("Absent", .data$Status)) %>%
    dplyr::select('eppocode', 'continent', 'country') %>%
    dplyr::group_by(.data$eppocode, .data$continent) %>%
    dplyr::distinct() %>%
    dplyr::mutate(distribution = paste(.data$country,
                                       collapse = ', ')) %>%
    dplyr::mutate(distribution = paste(.data$continent,
                                      .data$distribution,
                                      sep = ': ')) %>%
    dplyr::ungroup() %>%
    dplyr::select('eppocode', 'distribution') %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$eppocode) %>%
    dplyr::mutate(distribution = paste(.data$distribution,
                                       collapse = '; ')) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  return(list(list_table = distri_lists,
         compact_table = compact_table))
}


#' @rdname eppo_tabletools
#' @export
eppo_tabletools_pests <- function(names_tables, token) {
  if (!all(inherits(token, c('pestr_token')))) {
    message('Your token argument is not of pestr_token class.
            Please provide token created with create_eppo_token function')
  } else {
    #create reusable variables to access EPPO API
    eppocodes <- names_tables[[3]]$eppocode
#    pests_urls <-paste0('https://data.eppo.int/api/rest/1.0/taxon/',
#                        eppocodes, '/pests', token)
    #download data on pests from EPPO and strore them as list, name each list
    #element with eppocode and bind sub-tables by rows
    #to store them as long table
    pests_download <- eppo_rest_download(eppocodes, "pests", token)
    #exchange empty lists with NA table to avoid empty host table
     if(is.null(unlist(pests_download))) {
       pests_table <- data.frame(eppocode       = eppocodes,
                                 codeid         = names_tables[[3]]$codeid,
                                 pests_eppocode = NA,
                                 idclass        = NA,
                                 labelclass     = NA,
                                 fullname       = NA)
     } else {
       names(pests_download) <- eppocodes
       pests_table <- lapply(pests_download,
                             function(x) dplyr::bind_rows(x)) %>%
         dplyr::bind_rows(.id = 'host_code') %>%
         dplyr::rename(pests_eppocode = .data$eppocode,
                       eppocode = .data$host_code)
     }
    #take long table and colapse all the host names into one string,
    #separeted with names of host categories (major, minor, incidental etc.)
     compact_table <- pests_table %>%
       dplyr::group_by(.data$eppocode, .data$labelclass) %>%
       dplyr::select('eppocode', 'labelclass', 'fullname') %>%
       dplyr::mutate(pests = paste(.data$fullname,
                                   collapse = ', ')) %>%
       dplyr::mutate(pests = paste0(.data$labelclass, ': ', .data$pests)) %>%
       dplyr::ungroup() %>%
       dplyr::select('eppocode', 'pests') %>%
       dplyr::distinct() %>%
       dplyr::group_by(.data$eppocode) %>%
       dplyr::mutate(pests = paste(.data$pests, collapse = '; ')) %>%
       dplyr::distinct()

     return(list(long_table = pests_table,
                 compact_table))


  }
}
