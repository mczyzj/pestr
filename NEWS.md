# pestr 0.8.2

## Function updates

* `create_eppo_token` it does not create `Global variable` now. Instead user needs to assign it to a variable manually. This is also corrected in tests, vignettes and documentation.
* `eppo_tabletools_taxo` now correctly handles eppocodes with correct structure, but otherwise invalid. I.e. It returns `NA` when the eppocode is not in the **EPPO Data Services** instead of breaking.

# pestr 0.8.1

## New functions and tests

* `null_eppocodes` function checks if provided vector of eppocodes is empty, and returns `NULL` if needed. Function is tested. 

## Fixed issues

* `eppo_tabletools_` family functions now correctly handle results of `eppo_names_table`, when there are no valid eppocodes.
* refactor and adjust `eppo_database_download` fail gracefully.
* corrected typos in documentation

# pestr 0.8.0

## Vignettes

* updated `petr workflow` vignette.
* updated `Example workflow of checking taxons affecting Abies alba` vignette.

## New functions and updates

* `eppo_tabletools` functions now can use "raw" eppocodes instead of result of `eppo_names_tables` function result to query **EPPO Data Services** and **EPPO Global Database**. New functionality works with `raw_eppocodes` and 'use_raw_codes` parameters.
* `msg_helpers` small wrapper over messages to reduce redundancy in code.

## Fixed issues and smaller functionalities

* updated documentation of functions, including examples, typos, etc. (#11)
* added "devel" on Ubuntu 18.04 to github actions.
* removed `stringr` and `DT` from Suggested dependencies as the are no longer in need.

# pestr 0.7.4

## New and updated test.

* updated sample database (on 2020-Jan-06).(#19)
* updated `eppo_tabletools_hosts` test including changes in function behavior.
* if `eppo_token` is incorrect, functions return invisible `NULL`.(#18)

## Fixed issues and smaller functionalities

* Updated description of usage in *README*.
* **Changed license to MIT.**
* `eppo_tabletools_hosts` and `eppo_tabletools_pests` now include row with eppocode for which there is no data.

# pestr 0.7.3
## New functions

* `check_eppo_token` allows checking if `eppo_token` is correctly recognized by EPPO Data Services API.(#18)
* `try_GET` and `eppo_try_urls` helper wrappers to fail gracefully. (#18)
* Updated `eppo_json_wrapper` and `eppo_csv_download` helpers to fail gracefully.(#18)

## New and updated tests

* Updated tests include mocking to support testing on CI and CRAN without using token, or connect to **EPPO API**, or download *csv* files.
* **EPPO REST** responses are stored in RDS, so they can be mocked while testing if the creation of tables works correctly.(#19)
* Now test are only checking if it `inherits` from `pestr_token` class.
* mocked files and *SQLite* db is up to date on *04-Jan-2020*.(#19)
* Added tests for `eppo_tabletools_pests`.

## New vignettes

* Vignette showing how to use output of `eppo_tabletools_pests()` to check taxonomy of pest infecting *Abies alba*. (#8)

## Fixed issues and smaller functionalities

* Encrypted token for tests is no longer needed for all functions besides API helper since functions are mocked. (#12)
* Updated sample database.
* Updated `tidyr::nest` and dplyr functions arguments. (#20 #16)
* Deleted `dplyr` package from imports. Package now only uses `::` to access dplyr functions.
* Deleted `pryr` for dependencies at it was only used to check if `eppo_token` is `S3` class. 
* Deleted `RCurl` from dependencies since now package relies on `httr` and `curl` to connect with **REST API**, download *csv* files or download *SQLite* database.
* All test now use correct syntax to load packages (e.g. `library('dplyr')` instead of `library(dplyr)`).
* Added hex sticker.
* Added `lifcycle badges`

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

Added some formatting to NEWS.md

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

* `eppo_table_full` -- joins compact results of `eppo_names_tables()` and 
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
uses them to create table with preferred and non-preferred names, synonyms, and
common names in other languages;
* `eppo_tabletools_names()` -- takes result of `eppo_names_tables()` and creates
table with proper formatting (names etc.) on long or condensed format;

## Issues to solve

* Unify table outputs (#17)
* Encrypt EPPO API token so test could run also on TravisCI (#12) *FIXED*
* Code comments (#11)
* Documentation (#10)
* Additional tests (?) (#9)
* Test for `eppo_tabletools_taxo` breaks -- function does not return correct
values (namely table has only one row with NA) for compact table, however when
run manually everything is O.K. (#9) -- Probably updating sample SQLite db for
test will solve above issue *FIXED*
* Add new list item - tables formatted in the same way as they are formatted
in EPPO template (#7)
* Internationalization of package (#5) and (#6)
* Hosts and categorization paste some values double into string, small fix is
needed for both test and function *FIXED*

## TODOs

* `taxize` package can be helpful with correction of organism names.
