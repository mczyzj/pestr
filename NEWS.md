# pestr 0.6.0

## New functions

* `eppo_table_full` -- joins comapct results of `eppo_names_tables()` and 
`eppo_tabletools` into one compact table

# pestr 0.5.0

## New functions

* `eppo_tabletools_distri` -- takes result of `eppo_names_tables()` and creates
table with distribution of pests (continent and country level)

# pestr 0.4.0

## New functions

* `eppo_tabletools_cat()` -- takes result of `eppo_names_tables()` and creates
table with categorization of each pest

## Bug fixes

Fixed bug in `eppo_tabletools_hosts()` function. Now it returns correct values,
in compact table.

# pestr 0.3.0

## New functions

* `eppo_tabletools_cat()` -- takes result of `eppo_names_tables()` and creates
table with categorization of each pest

# pestr 0.2.0

## New functions

* `eppo_tabletools_hosts()` -- takes result of `eppo_names_tables()` and creates
table with hosts of each pest

# pestr 0.1.0

## New functions

* `create_eppo_token()` -- stores EPPO token for further use;
* `eppo_database_check()` -- checks if EPPO SQLite db exist in folder
* `eppo_database_connect()` -- connects to EPPO SQLite db
* `eppo_database_download()` -- downloads EPPO SQLite db from EPPO Data Services
* `eppo_names_tables()` -- checks if names in query exist in EPPO SQLite db and
uses them to creat table with preferred and non-preferred names, synonyms, and
common names in other languages
* `eppo_tabletools_names()` -- takes result of `eppo_names_tables()` and creates
table with proper formating (names etc.) on long or condensed format

## Issues to solve

* Documentation
* Code comments
* Additional tests (?)
* Encrypt EPPO API token so test could run also on TravisCI
* Test for `eppo_tabletools_taxo` breaks -- function does not return correct
values (namely tabel has only one row with NA) for compact table, however when
run manually everything is O.K.
