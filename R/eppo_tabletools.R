#' EPPO table manipulation tools
#'
#' `r lifecycle::badge("maturing")`
#' \code{eppo_tabletools_names} creates tables with names -- preferred, common
#' and synonyms -- of pests. It is particularly useful for creating whole table
#' with {\link{eppo_table_full}}, otherwise it might be more informative to use
#' {\link{eppo_names_tables}} which provides additional information for your
#' query.
#' \code{eppo_tabletools_hosts} creates tables with hosts of pests.
#' \code{eppo_tabletools_cat} creates tables with categorization of pests.
#' \code{eppo_tabletools_taxo} creates tables with taxonomy of pests/hosts.
#' \code{eppo_tabletools_distri} creates tables with distribution of pests.
#' \code{eppo_tabletools_pests} creates table with pests of host.
#' All functions return both long table and compact, human friendly table.
#'
#' @param names_tables A list of tables created via [eppo_names_tables()].
#' @param token An object containing EPPO API token created via
#'   [create_eppo_token()].
#' @param raw_eppocodes A character vector of eppocodes. Use with caution,
#'   and ONLY when sure that provided eppocodes are correct.
#' @param use_raw_codes logical. Default FALSE. Set TRUE if you want to provide
#' eppocodes directly.
#' @examples \dontrun{
#'  ## the code below requires SQLite DB in the working directory
#'  ## which can be downloaded with eppo_database_download function
#'  test_names <- eppo_names_tables(c("Xylella", "Poacea"))
#'  # get data on naming
#'  eppo_tabletools_names(test_names)
#'
#'  ## below functions connect to EPPO Global Database or EPPO Global Services
#'  ## they need connection to internet.
#'
#'  # get data on distribution
#'  eppo_tabletools_distri(test_names)
#'  # you can also get data using eppocodes directly
#'  eppo_tabletools_distri(raw_eppocodes = c("XYLEFA", "1GRAF"),
#'                         use_raw_codes = TRUE)
#'
#'  ## below functions, beside internet connection require also valid token that
#'  ## can be obtained after free of charge registration at EPPO Data Services
#'
#'  # first create token variable:
#'  create_eppo_token("paste_your_token_here")
#'
#'  # then use result of eppo_names_tables or raw eppocodes to query REST API
#'  # get data on hosts
#'  eppo_tabletools_hosts(test_names, eppo_token)
#'  eppo_tabletools_hosts(token = eppo_token,
#'                        raw_eppocodes = c("XYLEFA", "1GRAF"),
#'                        use_raw_codes = TRUE)
#'
#'  # get data on categorization
#'  eppo_tabletools_cat(test_names, eppo_token)
#'  eppo_tabletools_cat(token = eppo_token,
#'                        raw_eppocodes = c("XYLEFA", "1GRAF"),
#'                        use_raw_codes = TRUE)
#'
#'  # get data on taxonomy
#'  eppo_tabletools_taxo(test_names, eppo_token)
#'  eppo_tabletools_taxo(token = eppo_token,
#'                        raw_eppocodes = c("XYLEFA", "1GRAF"),
#'                        use_raw_codes = TRUE)
#'
#'  # get data on pests
#'  eppo_tabletools_pests(test_names, eppo_token)
#'  eppo_tabletools_pests(token = eppo_token,
#'                        raw_eppocodes = c("XYLEFA", "1GRAF"),
#'                        use_raw_codes = TRUE)
#' }
#' @return List containing two data frames. First is in a long format, and each
#'   row contains synonyms and names in other languages in respect to preferred
#'   names. The second data frame contains coerced synonyms and other names to
#'   single cell for each preferred name.
#' @seealso *EPPO Data services* \url{https://data.eppo.int/} and
#'   *EPPO Global Database* \url{https://gd.eppo.int/}
#' @name eppo_tabletools
NULL

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_names <- function(names_tables) {
  #intermediate table holding non preffered names with corrected headings
  other_table <- names_tables$all_associated_names %>%
    dplyr::filter(.data$preferred == 0) %>%
    dplyr::select(.data$codeid, Other_names = .data$fullname, .data$codelang)
  #long format table containing collumns for preffered names and accompanying
  #other names, also intermediate table that is a basis for a compact table
  preferred_table <- names_tables$all_associated_names %>%
    dplyr::filter(.data$preferred == 1) %>%
    dplyr::select(.data$codeid, .data$eppocode,
                  Preferred_name = .data$fullname) %>%
    dplyr::left_join(other_table, by = 'codeid') %>%
    dplyr::mutate(Name_type = ifelse(.data$codelang == 'la',
                                     'Synonym', 'Other languages')) %>%
    dplyr::mutate(dplyr::across(c(.data$Other_names, .data$codelang),
                                ~ ifelse(is.na(.x), "none", (.x)))) %>%
    dplyr::mutate(Name_type = ifelse(is.na(.data$Name_type),
                                     "Preferred", .data$Name_type)) %>%
    dplyr::arrange(.data$Preferred_name,
                   dplyr::desc(.data$Name_type),
                   .data$Other_names)

  # if the preffered table has no rows, terminate and return
  # list with 0-row preffered data frame and 0-row compact data frame

  if (dim(preferred_table)[1] == 0) {
    compact_table <- data.frame(
      codeid = character(),
      eppocode = character(),
      Preferred_name = character(),
      Other_names = character())
    message(msg_helper("empty_query"))
    return(list(long_table = preferred_table,
                compact_table = compact_table))
  }

  #temporary table nested by name type and other names, in next step
  #names will be collapsed to one cell per preffered name
  temp_table <- preferred_table %>%
    dplyr::select(
      .data$codeid, .data$eppocode,
      .data$Preferred_name, .data$Other_names, .data$Name_type
      ) %>%
    tidyr::nest(data = c(.data$Name_type, .data$Other_names))

  temp_Index <- seq(length(temp_table$data))

  for (i in 1:length(temp_table$data)) {
    temp_Index[i] <- temp_table$data[[i]] %>%
      dplyr::arrange(dplyr::desc(.data$Name_type), .data$Other_names) %>%
      dplyr::group_by(.data$Name_type) %>%
      dplyr::mutate(temp_names = paste(.data$Other_names, collapse = ', ')) %>%
      dplyr::distinct(.data$temp_names) %>%
      dplyr::mutate(temp_names = paste(.data$Name_type, .data$temp_names,
                                       sep = ': ')) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$temp_names) %>%
      dplyr::mutate(alt_names = paste(
        .data$temp_names[1], .data$temp_names[2], sep = '; '),
        .keep = "none"
        ) %>%
      dplyr::distinct() %>%
      gsub('; NA', '', .)
  }

  compact_table <- temp_table %>%
    dplyr::mutate(data = temp_Index) %>%
    dplyr::rename(Other_names = .data$data)

  return(list(long_table = preferred_table,
              compact_table = compact_table))
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_hosts <- function(names_tables = NULL,
                                  token, raw_eppocodes = NULL,
                                  use_raw_codes = FALSE) {
  if (!all(inherits(token, c('pestr_token')))) {
    message(msg_helper("wrong_token"))
    return(invisible(NULL))
  } else if (use_raw_codes & is.null(raw_eppocodes)) {
    message(msg_helper("wrong_arguments"))
    return(invisible(NULL))
  } else {
  #create reusable variables to access EPPO API
    if (!use_raw_codes) {
      #substitute empty with null
      eppocodes <- null_eppocodes(names_tables[[3]]$eppocode)
    } else {
      eppocodes <- check_eppocodes(raw_eppocodes)
    }

  #download data on hosts from EPPO and strore them as list, name each list
  #element with eppocode and bind sub-tables by rows to store them as long table
    if (is.null(eppocodes)) {
      message(msg_helper("empty_query"))
      return(invisible(NULL))
    }
    hosts_download <- eppo_rest_download(eppocodes, "hosts", token)
  #empty table to substitute
    empty_host_df <- data.frame(
      codeid = NA,
      eppocode = NA,
      idclass = 9,
      labelclass = "Host",
      full_name = NA
    )

  #subsitute empty list elements with empty table so the eppocodes
  #for which there are no results are included in function output
    for(i in 1:length(hosts_download)) {
      if(rlang::is_empty(hosts_download[[i]])) {
        hosts_download[[i]] <- list(Host = empty_host_df)
      }
    }
    names(hosts_download) <- eppocodes
    hosts_table <- lapply(hosts_download,
                          function(x) dplyr::bind_rows(x)) %>%
      dplyr::bind_rows(.id = 'pest_code') %>%
      dplyr::rename(host_eppocode = .data$eppocode,
                    eppocode      = .data$pest_code)
  #take long table and collapse all the host names into one string,
  #separated with names of host categories (major, minor, incidental etc.)
    compact_table <- hosts_table %>%
      dplyr::select(.data$eppocode, .data$labelclass, .data$full_name) %>%
      dplyr::group_by(.data$eppocode, .data$labelclass) %>%
      dplyr::mutate(hosts = paste0(.data$labelclass, ': ',
                                   paste(.data$full_name, collapse = ', '))) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$eppocode, .data$hosts) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$eppocode) %>%
      dplyr::mutate(hosts = paste(.data$hosts, collapse = '; ')) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    return(list(long_table = hosts_table,
                compact_table = compact_table))
  }
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_cat <- function(names_tables = NULL,
                                token, raw_eppocodes = NULL,
                                use_raw_codes = FALSE) {
  if (!all(inherits(token, c('pestr_token')))) {
    message(msg_helper("wrong_token"))
    return(invisible(NULL))
  } else if (use_raw_codes & is.null(raw_eppocodes)) {
    message(msg_helper("wrong_arguments"))
    return(invisible(NULL))
  } else {
    #create reusable variables to access EPPO API
    if (!use_raw_codes) {
      #substitute empty with null
      eppocodes <- null_eppocodes(names_tables[[3]]$eppocode)
    } else {
      eppocodes <- check_eppocodes(raw_eppocodes)
    }
  #download data on categorization from EPPO and strore them as list of tables
    if (is.null(eppocodes)) {
      message(msg_helper("empty_query"))
      return(invisible(NULL))
    }
    cat_list_table <- eppo_rest_download(eppocodes, "categorization", token)
    cat_tables <- setNames(vector("list", length(eppocodes)), eppocodes)

    #empty table to substitute
    empty_cat_df <- data.frame(
      nomcontinent = NA,
      isocode      = NA,
      country      = NA,
      qlist        = NA,
      qlistlabel   = NA,
      yr_add       = NA,
      yr_del       = NA,
      yr_trans     = NA
      )
  #subsitute empty list elements with empty table so the eppocodes
  #for which there are no results are included in function output
  #-> NEEDS REFACTORING!!!
    for (i in 1:length(cat_list_table)) {
      if (rlang::is_empty(cat_list_table[[i]]) == TRUE) {
        cat_tables[[i]] <- empty_cat_df
      } else {
        cat_tables[[i]] <- cat_list_table[[i]]
      }
    }

    long_table <- cat_tables %>%
      dplyr::bind_rows(.id = 'eppocode')
  #collapse values in list tables into whole categorization in one cell
  #for each of the pests
    compact_list <- setNames(vector("list", length(eppocodes)), eppocodes)

    for (i in 1: length(cat_tables)) {
      compact_list[[i]] <- cat_tables[[i]] %>%
        tidyr::nest(data = .data$nomcontinent) %>%
        dplyr::mutate(categorization = paste0(.data$country, ': ',
                                              .data$qlistlabel, ': ',
                                              'add/del/trans: ',
                                              .data$yr_add, '/',
                                              .data$yr_del, '/',
                                              .data$yr_trans)) %>%
        tidyr::unnest(cols = .data$data) %>%
        dplyr::select(.data$nomcontinent, .data$categorization) %>%
        dplyr::group_by(.data$nomcontinent) %>%
        dplyr::mutate(categorization = paste(.data$categorization,
                                             collapse = '; ')) %>%
        dplyr::distinct(.data$categorization) %>%
        dplyr::mutate(categorization = paste(.data$nomcontinent,
                                             .data$categorization,
                                             sep = ': ')) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(categorization = paste(.data$categorization,
                                             collapse = ' | '),
                      .keep = "none") %>%
         dplyr::distinct()
    }

    compact_table <- compact_list %>%
      dplyr::bind_rows(.id = 'eppocode')

    return(list(long_table = long_table,
                compact_table = compact_table))
  }
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_taxo <- function(names_tables = NULL,
                                 token, raw_eppocodes = NULL,
                                 use_raw_codes = FALSE) {
  if (!all(inherits(token, c('pestr_token')))) {
    message(msg_helper("wrong_token"))
    return(invisible(NULL))
  } else if (use_raw_codes & is.null(raw_eppocodes)) {
    message(msg_helper("wrong_arguments"))
    return(invisible(NULL))
  } else {
  #create reusable variables to access EPPO API
    if (!use_raw_codes) {
      #substitute empty with null
      eppocodes <- null_eppocodes(names_tables[[3]]$eppocode)
    } else {
      eppocodes <- check_eppocodes(raw_eppocodes)
    }
  #download data on taxonomy from EPPO and strore them as list of tables
    if (is.null(eppocodes)) {
      message(msg_helper("empty_query"))
      return(invisible(NULL))
    }
    taxo_list_table <- eppo_rest_download(eppocodes, "taxonomy", token)
  #empty table to substitute
    empty_taxo_df <- data.frame(codeid = NA,
                                eppocode = NA,
                                prefname = NA,
                                level = NA,
                                stringsAsFactors = FALSE)

    taxo_tables <- stats::setNames(vector("list", length(eppocodes)), eppocodes)
  #subsitute empty list elements with empty table so the eppocodes
  #for which there are no results are included in function output
    for (i in 1:length(taxo_list_table)) {
      if (rlang::is_empty(taxo_list_table[[i]]) == TRUE) {
        taxo_tables[[i]] <- empty_taxo_df
      } else {
        taxo_tables[i] <- taxo_list_table[i]
      }
    }

    long_table <- taxo_tables %>%
      dplyr::bind_rows(.id = "eppocode")

  #Create compact table
    compact_table <- data.frame(eppocode = eppocodes,
                                taxonomy = rep(NA, length(eppocodes)),
                                stringsAsFactors = FALSE)
  #Shorten the table to show only sensible taxonomic level,
  #this might look better if the conditionals are exchanged with
  #case_when function
    for (j in 1:length(taxo_tables)) {
      if(is.na(taxo_tables[[j]][1,1])) {
        compact_table[j, 2] <- NA
      } else if(taxo_tables[[j]][1, 3] == 'Animalia') {
        compact_table[j, 2] <- taxo_tables[[j]][2, 3]
      } else if (taxo_tables[[j]][1, 3] == 'Viruses and viroids') {
        compact_table[j, 2] <- taxo_tables[[j]][2, 3]
    #  } else if (is.na(taxo_tables[[j]][1, 3])) {
    #    compact_table[j, 2] <- "xyz"
      } else {
        compact_table[j, 2] <- taxo_tables[[j]][1, 3]
      }
    }

    return(list(long_table = long_table,
                compact_table = compact_table))
  }
}

#' @rdname eppo_tabletools
#' @export
eppo_tabletools_distri <- function(names_tables = NULL,
                                   raw_eppocodes = NULL,
                                   use_raw_codes = FALSE) {
  if (use_raw_codes & is.null(raw_eppocodes)) {
    message(msg_helper("wrong_arguments"))
    return(invisible(NULL))
  } else {
    #create reusable variables to access EPPO API
    if (!use_raw_codes) {
      #substitute empty with null
      eppocodes <- null_eppocodes(names_tables[[3]]$eppocode)
    } else {
      eppocodes <- check_eppocodes(raw_eppocodes)
    }
  }
  #Download data on distribution and store them in list of tables
  if (is.null(eppocodes)) {
    message(msg_helper("empty_query"))
    return(invisible(NULL))
  }
  distri_urls <- paste0('https://gd.eppo.int/taxon/',
                       eppocodes,'/download/distribution_csv')
  distri_lists <- eppo_csv_download(eppocodes)

  #empty table to substitute
  empty_distri_df <- data.frame(
    continent    = NA,
    country      = NA,
    state        = NA,
    country.code = NA,
    state.code   = NA,
    Status       = NA
    )
  #subsitute empty list elements with empty table so the eppocodes
  #for which there are no results are included in function output
  for (i in 1:length(distri_lists)) {
    if (dim(distri_lists[[i]])[1] == 0) {
      distri_lists[[i]] <- empty_distri_df
    }
  }

  long_table <- distri_lists %>%
    dplyr::bind_rows(.id = "eppocode")
  #take long table and colapse all the distributions into
  #one string per eppocode
  compact_table <- long_table %>%
    dplyr::filter(!grepl("Absent", .data$Status)) %>%
    dplyr::select(.data$eppocode, .data$continent, .data$country) %>%
    dplyr::group_by(.data$eppocode, .data$continent) %>%
    dplyr::distinct() %>%
    dplyr::mutate(distribution = paste(.data$country,
                                       collapse = ', ')) %>%
    dplyr::mutate(distribution = paste(.data$continent,
                                      .data$distribution,
                                      sep = ': ')) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$eppocode, .data$distribution) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$eppocode) %>%
    dplyr::mutate(distribution = paste(.data$distribution,
                                       collapse = '; ')) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  return(list(long_table = long_table,
              compact_table = compact_table))
}


#' @rdname eppo_tabletools
#' @export
eppo_tabletools_pests <- function(names_tables = NULL,
                                  token, raw_eppocodes = NULL,
                                  use_raw_codes = FALSE) {
  if (!all(inherits(token, c('pestr_token')))) {
    message(msg_helper("wrong_token"))
    return(invisible(NULL))
  } else if (use_raw_codes & is.null(raw_eppocodes)) {
    message(msg_helper("wrong_arguments"))
    return(invisible(NULL))
  } else {
    #create reusable variables to access EPPO API
    if (!use_raw_codes) {
      #substitute empty with null
      eppocodes <- null_eppocodes(names_tables[[3]]$eppocode)
    } else {
      eppocodes <- check_eppocodes(raw_eppocodes)
    }
  #download data on pests from EPPO and strore them as list, name each list
  #element with eppocode and bind sub-tables by rows
  #to store them as long table
    if (is.null(eppocodes)) {
      message(msg_helper("empty_query"))
      return(invisible(NULL))
    }
    pests_download <- eppo_rest_download(eppocodes, "pests", token)
  #empty table to substitute
    empty_pests_df <- data.frame(
      eppocode = NA,
      idclass = 9,
      labelclass = "Host",
      fullname = NA
    )

    names(pests_download) <- eppocodes
  #subsitute empty list elements with empty table so the eppocodes
  #for which there are no results are included in function output
    for(i in 1:length(pests_download)) {
      if(rlang::is_empty(pests_download[[i]])) {
        pests_download[[i]] <- list(Host = empty_pests_df)
      }
    }

    pests_table <- lapply(pests_download,
                          function(x) dplyr::bind_rows(x)) %>%
      dplyr::bind_rows(.id = 'host_code') %>%
      dplyr::rename(pests_eppocode = .data$eppocode,
                    eppocode = .data$host_code)
  #take long table and colapse all the host names into one string,
  #separeted with names of host categories (major, minor, incidental etc.)
    compact_table <- pests_table %>%
      dplyr::group_by(.data$eppocode, .data$labelclass) %>%
      dplyr::select(.data$eppocode, .data$labelclass, .data$fullname) %>%
      dplyr::mutate(pests = paste(.data$fullname,
                                  collapse = ', ')) %>%
      dplyr::mutate(pests = paste0(.data$labelclass, ': ', .data$pests)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$eppocode, .data$pests) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$eppocode) %>%
      dplyr::mutate(pests = paste(.data$pests, collapse = '; ')) %>%
      dplyr::distinct()

    return(list(long_table = pests_table,
                compact_table = compact_table))
  }
}
