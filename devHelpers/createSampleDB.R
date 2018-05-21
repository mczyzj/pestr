testDB <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'eppo_sample.sqlite')

names_vector <- c('Cydia packardi', 'noPest', 'cherry virus', 'noPest2',
                  'Xylella', 'Drosophila')

eppoDB <- dbConnect(RSQLite::SQLite(), dbname = 'eppocodes.sqlite')

t_names_tab <- eppoDB %>%
  DBI::dbGetQuery(paste0('SELECT * FROM t_names WHERE fullname LIKE ',
             paste(paste0("'%", names_vector, "%'"),
             collapse = " OR fullname LIKE ")))

t_codes_tab <- eppoDB %>%
  DBI::dbGetQuery(paste0('SELECT * FROM t_codes WHERE codeid IN (',
                         paste0(unique(t_names_tab$codeid), collapse = ', '),
                         ')'))

t_names_all <- eppoDB %>%
  DBI::dbGetQuery(paste0('SELECT * FROM t_names WHERE codeid IN (',
                         paste(unique(t_names_tab$codeid), collapse = ', '),
                         ')'))

dbCreateTable(testDB, "t_names", t_names_tab)
dbCreateTable(testDB, "t_codes", t_codes_tab)

dbWriteTable(testDB, "t_names", t_names_all, overwrite = T)
dbWriteTable(testDB, "t_codes", t_codes_tab, overwrite = T)
