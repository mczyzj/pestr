#' Messages helper tools
#'
#' Messages wrapper. Helps with printing nicely and reduces redundancy in code.
#'
#' @param x message code name.
#' @param ... other parameters passed to functions.
#'
#' @return String to be used by message functions.
#' @noRd
msg_helper <- function(x, ...) {
  switch(
    x,
    "wrong_token" = paste0(
      'Your token argument is not of pestr_token class.\n',
      'Please provide token created with create_eppo_token function'
    ),
    "forbidden_chars" = paste0(
      'Token contains forbiden characters'
    ),
    "wrong_arguments" = paste0(
      'Please provide character vector of eppocodes or set use_raw_codes\n',
      'parameter to FALSE and pass result of eppo_names_table function to\n',
      'names_tables parameter.'
    ),
    "empty_query" = paste0(
      'All provided eppocodes have incorrect stucture\n',
      'or the result of eppo_names_tables is empty.\n',
      'Please provide at least one valid eppocode.'
    ),
    "no_db" = paste0(
      'There is no such database in location you provided.\n',
      'Please provide correct name for database and/or location\n',
      'or use eppo_database_download to download .zip files from\n',
      'EPPO Data Services.'
    ),
    "no_file" = paste0(
      'This file does not exist,\n',
      'please download or give correct directory'
      ),
    "db_connection" = paste0(
      'Your connection to EPPO SQLite database is etablished'
    ),
    "db_win_unzip" = paste0(
      'Please unzip sqllite.zip file manually to your working directory'
    ),
    "wrong_eppocodes" = paste0(
      'Following codes have wrong structure and will not be used.\n',
      'Please check and correct if needed.\n',
      paste(..., collapse = ' | ')
    ),
    "no_distri" = paste0(
      'The distribution file for EPPO code '
      , ...,
      ' was not found.'
      ),
    "db_time" = paste0(
      'EPPO database was downloaded on ',
        as.character(file.info(...)$mtime),
      "\n",
      "You might consider updating."
    ),
    "no_download" = paste0(
      'The file was not downloaded. Possible problem: cannot connect to server.'
    )
  )
}
