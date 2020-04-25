# pestr 0.7.3

## New and updated tests

* Updated tests include mocking to support testing on CI and CRAN without using token, or connect to EPPO API, or download *csv* files.
* EPPO REST responses are stored in RDS, so they can be mocked while testing if the creation of tables works correctly.
* Now test are only checking if it `inherits` from `pestr_token` class.

## New vignettes

* Vignette showing how to use output of `eppo_tabletools_pests()` to check taxonomy of pest infecting Abies alba.

## Fixed issues

* Encrypted token for tests is no longer needed for all functions besides API helper since functions are mocked. #12
* Updated sample db.
* Updated tidyr::nest and dplyr functions arguments.
* Deleted `dplyr` package from imports. Package now only uses `::` to acces dplyr functions.
* Deleted `pryr` for dependencies at it was only used to check if `eppo_token` is `S3` class. 
* Deleted `RCurl` from dependecies since now package relies on `httr` to connect with **REST API**.
* All test now use correct syntac to load packages (e.g. library('dplyr') instead of `library(dplyr)`).
* Added hex sticker.

# pestr 0.7.2

## New functions

* new function `eppo_tabletools_pests` returning pests of hosts.

## Known issues

* `eppo_tabletools_pests` needs testing.

# pestr 0.7.1

## Fixed issues
  
* updated comment in `eppo_names_table`
* deleted unused dependency from vignette
* added condition to download db file only if one does not exist in `eppo_database_download`

# pestr 0.7.0

## New vignette

Added *pestr Workflow* vignette with example usage of **pestr** package functions -- fix (#8).

# Fixed issues

Added some formating to NEWS.md

# pestr 0.6.4

## Fixed issues

Corrected table name in `eppo_tabletools_host` function.

# pestr 0.6.3

## Fixed issues

`eppo_tabletools_host` -- some small fixes in code and now returns table
with NA, when there is no record in DB.

# pestr 0.6.2

## Fixed issues

`eppo_tabletools_hosts` correctly joins hosts names with their classes.

# pestr 0.6.1

## Fixed issues

`eppo_tabletools_distri` correctly joins countries with continents, without
repeating names of some countries (e.g. *USA*, *China*).


# pestr 0.6.0

## New functions

* `eppo_table_full` -- joins comapct results of `eppo_names_tables()` and 
`eppo_tabletools` into one compact table.

# pestr 0.5.0

## New functions

* `eppo_tabletools_distri` -- takes result of `eppo_names_tables()` and creates
table with distribution of pests (continent and country level).

# pestr 0.4.0

## New functions

* `eppo_tabletools_cat()` -- takes result of `eppo_names_tables()` and creates
table with categorization of each pest.

## Bug fixes

Fixed bug in `eppo_tabletools_hosts()` function. Now it returns correct values,
in compact table.

# pestr 0.3.0

## New functions

* `eppo_tabletools_cat()` -- takes result of `eppo_names_tables()` and creates
table with categorization of each pest.

# pestr 0.2.0

## New functions

* `eppo_tabletools_hosts()` -- takes result of `eppo_names_tables()` and creates
table with hosts of each pest.

# pestr 0.1.0

## New functions

* `create_eppo_token()` -- stores EPPO token for further use;
* `eppo_database_check()` -- checks if EPPO SQLite db exist in folder;
* `eppo_database_connect()` -- connects to EPPO SQLite db;
* `eppo_database_download()` -- downloads EPPO SQLite db from EPPO Data Services;
* `eppo_names_tables()` -- checks if names in query exist in EPPO SQLite db and
uses them to creat table with preferred and non-preferred names, synonyms, and
common names in other languages;
* `eppo_tabletools_names()` -- takes result of `eppo_names_tables()` and creates
table with proper formating (names etc.) on long or condensed format;

## Issues to solve

* Encrypt EPPO API token so test could run also on TravisCI (#12)
* Code comments (#11)
* Documentation (#10)
* Additional tests (?) (#9)
* Test for `eppo_tabletools_taxo` breaks -- function does not return correct
values (namely tabel has only one row with NA) for compact table, however when
run manually everything is O.K. (#9) -- Probably updating sample SQLite db for
test will solve above issue
* Add new list item - tables formatted in the same way as they are formatted
in EPPO template (#7)
* Internationalization of package (#5) and (#6)
* Hosts and categorization paste some values double into string, small fix is
needed for both test and function

## TODOs

* Add vignettes.
* `taxize` package can be helpful with correction of organism names.
