#' EPPO Data Services Database connection tools
#'
#' \code{eppo_databse_check} checks if there is a file \emph{eppocodes.sqlite}
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
  download.file('https://data.eppo.int/files/sqlite.zip', destfile = zipfile)
  unzip(zipfile, overwrite = T)
}

#' @rdname eppo_database
#' @export
eppo_database_connect <- function(filepath = getwd(),
                                  filename = 'eppocodes.sqlite') {
  dbfile <- paste0(filepath, '/', filename)
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
#' \code{eppo_organism_check} checks if the names (or parts of the names) from
#' vector are present in the database. \code{eppo_organism_prefnames} extracts
#' preffered names of organisms from database. \code{eppo_organism_prefnames}
#' extracts codeids from t_names table from EPPO RSQLite database.
#'
#' @param names A vector with organism names (or part of the names) to be
#'    checked for existence in EPPO SQLite Database.
#' @param sqlEppo connection to SQLite EPPO Database. By default NULL, function
#' will automatically connect to database with default credentials.
#' @return Check if provided names are present in EPPO SQLite Database, creates
#'    data frame with preffered name and codeid.
#' @name eppo_organism_names
NULL

eppo_organism_check <- function(names_vector, sqlConnection = NULL) {
  #check if user provided db conection
  if (is.null(sqlConnection)) {
    sqlConnection <- eppo_database_connect()
  }
  #extract entries from SQLite db that match names in names_vector
  names_in_DB <- sqlConnection %>%
    DBI::dbGetQuery(paste0('SELECT codeid, fullname, preferred FROM t_names
                           WHERE fullname LIKE ',
                           paste(paste0("'%", names_vector, "%'"),
                                 collapse = " OR fullname LIKE ")))
  #intermediet list with that checks if element of names_vector
  #match element in SQLite db
  test_list <- lapply(names_vector, grep, names_in_DB$fullname)

  return(list(exist_in_DB = data.frame(names_in_DB),
       not_in_DB   = names_vector[lapply(names_vector,
                                         grep,
                                         names_in_DB$fullname) %>%
                                    sapply(length) == 0]))
}
