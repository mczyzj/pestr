#' EPPO Data Services Database connection tools
#'
#' `r lifecycle::badge("stable")`
#' \code{eppo_database_check} checks if there is a file *eppocodes.sqlite*
#' and informs user if it is outdated and should be downloaded.
#' \code{eppo_database_download} downloads database in *SQLite* format directly.
#' The downloaded file is *zip* archive. On Linux this database will be
#' extracted automatically. On Windows user will need to extract the file
#' manually.
#' \code{eppo_database_connect} allows user to connect to SQLite database
#' downloaded from EPPO Data Services.
#' @details # Manual download
#' If you will, you can download database directly from
#' **EPPO Data Services** \url{https://data.eppo.int}
#'
#' @param filepath A string with path where eppocodes.sqlite file is stored. By
#'    default it points to working directory.
#' @param filename A string with name of the file. By default it takes name
#'    eppocodes.sqlite.
#' @return Checks if database file exist in directory, if it is outdated, and
#'    establishes SQLite database connection
#' @examples
#' \dontrun{
#' #to check if the db file exist in the directory (default working directory)
#'
#' eppo_database_check(filepath = getwd())
#'
#' #to download EPPO SQLite database into directory (default working directory).
#' #If you are Windows user, after download finishes you will need to unzip file
#' #manualy.
#'
#' eppo_database_download(filepath = getwd())
#'
#' #prior to use functions that check pest names in SQLite database,
#' #you need to set up connection to SQLite database. Doing so is
#' #straightforward with function below (once per sesion):
#'
#' eppo_connection <- eppo_database_connect(filepath = getwd(),
#'                                          filename = "eppocodes.sqlite")
#' }
#' @name eppo_database
NULL

#' @rdname eppo_database
#' @export
eppo_database_check <- function(filepath = getwd(),
                                filename = 'eppocodes.sqlite') {
  dbfile <- utils::capture.output(cat(filepath, filename,
                                  sep = ifelse(.Platform$OS.type == 'windows',
                                            "\\", "/")))
  if (file.exists(dbfile)) {
         message(msg_helper("db_time", dbfile))
    return(TRUE)
  } else {
         message(msg_helper("no_file"))
  }
}

#' @rdname eppo_database
#' @export
eppo_database_download <- function(filepath = getwd()) {
  zipfile <- paste(filepath, 'eppocodes.zip',
                    sep = ifelse(.Platform$OS.type == 'windows', "\\", "/"))
  link <- 'https://data.eppo.int/files/sqlite.zip'
  ### Try to download zipfile, if somethings wrong fail gracefully
  #eppo_database_helper(zipfile = zipfile, link = link)

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
    # Then print curl download status
    resp <- try_GET(link)
    message(resp)
  }
    # If file exists unpack
  if (file.exists(zipfile)) {
    if(.Platform$OS.type == 'windows') {
      message(msg_helper("db_win_unzip"))
    } else {
      utils::unzip(zipfile, overwrite = T)
    }
  } else {
      message(msg_helper("no_download"))
  }
}

#' @rdname eppo_database
#' @export
eppo_database_connect <- function(filepath = getwd(),
                                  filename = 'eppocodes.sqlite') {
  dbfile <- utils::capture.output(cat(filepath, filename,
                                  sep = ifelse(.Platform$OS.type == 'windows',
                                            "\\", "/")))
  if (file.exists(dbfile)) {
    message(msg_helper("db_connection"))
    return(RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbfile))
  } else {
    message(msg_helper("no_db"))
    #result <- FALSE #this value is only needed for automatic testing
  }
}

#' EPPO Data Services SQLite Database search tools
#'
#' `r lifecycle::badge("maturing")`
#' \code{eppo_names_table} checks if string (names or parts of the names)
#' provided by user match any name in the EPPO database. If strings match any
#' name in database, data frame with all names matching codeid, as well as
#' preferred (binary), language and EPPOcode is returned.
#'
#' @details # Using with other functions
#' Output of the function might be passed to functions:
#' * \code{eppo_tabletools_names}
#' * \code{eppo_tabletools_cat}
#' * \code{eppo_tabletools_hosts}
#' * \code{eppo_tabletools_distribution}
#'
#' @param names_vector A vector with organism or viruses names
#'   (or part of the names) to be checked for existence in EPPO SQLite Database.
#' @param sqlConnection connection to SQLite EPPO Database. By default NULL;
#'    function will automatically connect to database with default credentials.
#' @return Check if provided names are present in EPPO SQLite Database. Creates
#'    list which contains: data frame with matching names in database and their
#'    codeids, character vector of names from \code{names_vector} which do not
#'    match any entry in database, data frame with preferred names and their
#'    codeids, and data frame containing all names matching codeids of preferred
#'    names. Last data frame contains also column with preferred (binary),
#'    codelang (two letter character with language code), and EPPOcode.
#' @examples \dontrun{
#'   ##This code needs SQLite dabatase in working directory
#'   test_names <- c("Xylella", "dog", "leafhopper")
#'   test_names_table <- eppo_names_table(test_names)
#' }
#' @name eppo_names_tables
#' @export
NULL

eppo_names_tables <- function(names_vector, sqlConnection = NULL) {
  #check if user provided db conection
  if (is.null(sqlConnection)) {
    sqlConnection <- eppo_database_connect()
  }
  #extract entries from SQLite db that match names in names_vector
  names_in_DB <- sqlConnection %>%
    DBI::dbGetQuery(paste0(
    'SELECT codeid, fullname FROM t_names WHERE fullname LIKE ',
    paste(paste0("'%", names_vector, "%'"),
          collapse = " OR fullname LIKE "))
    )
  #intermediet list that checks if element of names_vector
  #match element in SQLite db
  test_list <- lapply(names_vector, grep, names_in_DB$fullname)
  #extracts EPPO codes for unique codeid, that will be used in next
  #step to match names from db to unique eppocode
  EPPOcodes <- sqlConnection %>%
    DBI::dbGetQuery(paste0(
    'SELECT codeid, eppocode FROM t_codes WHERE codeid IN (',
    paste0(unique(names_in_DB$codeid), collapse = ', '), ')')
    )
  #creates data frame with all names that match codeid from names_in_DB
  all_names <- sqlConnection %>%
    DBI::dbGetQuery(paste0(
    'SELECT codeid, fullname, preferred, codelang, status
    FROM t_names WHERE codeid IN (',
    paste(unique(names_in_DB$codeid),
          collapse = ', '), ')')
    ) %>%
    dplyr::filter(.data$status == 'A')

  return(list(exist_in_DB = data.frame(names_in_DB),
              not_in_DB = names_vector[test_list %>% sapply(length) == 0],
              pref_names = data.frame(
                all_names %>%
                  dplyr::filter(.data$preferred == 1) %>%
                  dplyr::select(.data$codeid, .data$fullname) %>%
                  dplyr::left_join(EPPOcodes, by = 'codeid')
                ),
              all_associated_names  = data.frame(
                all_names %>%
                  dplyr::select(-.data$status) %>%
                  dplyr::left_join(EPPOcodes, by = 'codeid')))
         )
}
