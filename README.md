# pestr
Functions included in this package allows users to painlessly connect to and
extract data from EPPO Data Services. Before you start using it you should
register on: [EPPO Data Services](https://data.eppo.int/) and obtain your token
to access REST API.

Package include function that allow you to download SQLite database
(around 12 MB) that is needed for extracting eppocodes that are used in other
functions from this package. Function included in `eppo_tabletools` group return
both list of machine friendly list of tables and processed, compact table, that
contain all infromation in one row per one pest.

Before using functions that connect to REST API (hosts, categorization, taxonomy)
you should execute `create_eppo_token()` function with string argument equal to
your personal EPPO Data Services token. This function creates global variable
`eppo_token` which should be parsed as an argument to functions that require
`token` argument.

`eppo_table_full()` allow to execute all the functions and return compact table
with information on names, hosts, categorization, distribution and taxonomy --
one row per one pest.

Feel free to contribute to this package and report issues via GitHub or email.

[![Build Status](https://travis-ci.com/mczyzj/pestr.svg?branch=master)](https://travis-ci.com/mczyzj/pestr)
[![codecov](https://codecov.io/gh/mczyzj/pestr/branch/master/graph/badge.svg)](https://codecov.io/gh/mczyzj/pestr)


TODO:

* Code comments
* Encrypt token to make more test using TravisCI
* Update sample SQLite db
* Add Vignettes
