#' EPPO Data Services Database connection tools
#'
#' \code{eppo_database_check} checks if there is a file \emph{eppocodes.sqlite}
#' and informs user if it is outdated and should be downloaded.
#' \code{eppo_database_download} downloads database in SQLite format directly.
#' \code{eppo_databse_connect} allows user to conect to SQLite database
#' downloaded from EPPO Data Services.
#'
#' @param filepath A string with path where eppocodes.sqlite file is stored. By
#'    default it points to working directory.
#' @param filename A string with name of the file. By default it takes name
#'    eppocodes.sqlite.
#' @return Checks if database file exist in directory, if it is outdated, and
#'    establishes SQLite database connection
#' @name eppo_database
NULL

#' @rdname eppo_database
#' @export
eppo_database_check <- function(filepath = getwd(),
                                filename = 'eppocodes.sqlite') {
  dbfile <- paste0(filepath, '/', filename)
#  dbfile <- cat(filepath, filename,
#                sep = ifelse(.Platform$OS.type == 'windows', "\\", "/"))
  if (file.exists(dbfile)) {
         message(cat('EPPO database was downloaded on ',
                     as.character(file.info(dbfile)$mtime)))
  } else {
         message('This file does not exist,
                 please download or give correct directory')
  }
}

#' @rdname eppo_database
#' @export
eppo_database_download <- function(filepath = getwd()) {
  zipfile <- paste0(filepath, '/', 'eppocodes.zip')
#  zipfile <- cat(filepath, 'eppocodes.zip',
#                 sep = ifelse(.Platform$OS.type == 'windows', "\\", "/"))
  utils::download.file('https://data.eppo.int/files/sqlite.zip',
                       destfile = zipfile)
  utils::unzip(zipfile, overwrite = T)
}

#' @rdname eppo_database
#' @export
eppo_database_connect <- function(filepath = getwd(),
                                  filename = 'eppocodes.sqlite') {
  dbfile <- paste0(filepath, '/', filename)
  #  dbfile <- cat(filepath, filename,
  #                sep = ifelse(.Platform$OS.type == 'windows', "\\", "/"))
  if (file.exists(dbfile)) {
    message('Your connection to EPPO SQLite database is etablished')
    return(RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbfile))
  } else {
    message('There is no such database in location you provided.\n',
            'Please provide correct name for database and/or location\n',
            'or use eppo_database_download to download .zip files from\n',
            'EPPO Data Services.')
    #result <- FALSE #this value is only needed for automatic testing
  }
}

#' EPPO Data Services SQLite Database search tools
#'
#' \code{eppo_names_table} checks if string (names or parts of the names)
#' provided by user match any name in the EPPO database. If strings match any
#' name in database, data frame with all names matching codeid, as well as
#' preferred (binary), language and EPPOcode is returned. In order to use
#' functions: \code{eppo_tabletools_names}, \code{eppo_tabletools_cat},
#' \code{eppo_tabletools_hosts} or \code{eppo_tabletools_distribution}, you need
#' to run this function first.
#'
#' @param names_vector A vector with organism names (or part of the names) to be
#'    checked for existence in EPPO SQLite Database.
#' @param sqlConnection connection to SQLite EPPO Database. By default NULL;
#'    function will automatically connect to database with default credentials.
#' @return Check if provided names are present in EPPO SQLite Database, creates
#'    list which contains: data frame with matching names in database and their
#'    codeids, character vector of names from \code{names_vector} which do not
#'    match any entry in database, data frame with preferred names and their
#'    codeids, and data frame containing all names matching codeids of preffered
#'    names. Last data frame contains also column with preferred (binary),
#'    codeland (two letter character with language code), and EPPOcode.
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
    DBI::dbGetQuery(paste0('SELECT codeid, fullname FROM t_names
                           WHERE fullname LIKE ',
                           paste(paste0("'%", names_vector, "%'"),
                                 collapse = " OR fullname LIKE ")))
  #intermediet list with that checks if element of names_vector
  #match element in SQLite db
  test_list <- lapply(names_vector, grep, names_in_DB$fullname)
  #extracts EPPO codes for unique codeid
  EPPOcodes <- sqlConnection %>%
    DBI::dbGetQuery(paste0('SELECT codeid, eppocode FROM t_codes
                           WHERE codeid IN (',
                           paste0(unique(names_in_DB$codeid), collapse = ', '),
                           ')'))
  #creates data frame with all names that match codeid from names_in_DB
  all_names <- sqlConnection %>%
    DBI::dbGetQuery(paste0('SELECT codeid, fullname, preferred, codelang, status
                           FROM t_names WHERE codeid IN (',
                           paste(unique(names_in_DB$codeid), collapse = ', '),
                           ')')) %>%
    filter(.data$status == 'A')


  return(list(exist_in_DB     = data.frame(names_in_DB),
              not_in_DB       = names_vector[test_list %>%
                                               sapply(length) == 0],
              pref_names = data.frame(all_names %>%
                                        filter(.data$preferred == 1) %>%
                                        select(.data$codeid, .data$fullname) %>%
                                        left_join(EPPOcodes, by = 'codeid')),
              all_associated_names  = data.frame(all_names %>%
                                                   select(-.data$status) %>%
                                                   left_join(EPPOcodes,
                                                             by = 'codeid'))))
}
