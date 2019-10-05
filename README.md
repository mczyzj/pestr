# pestr
Functions included in this package allows users to painlessly connect to and
extract data from EPPO Data Services. Before you start using it you should
register on: [EPPO Data Services](https://data.eppo.int/) and obtain your token
to access REST API.

## Installation

You can install and use **pestr** development version from GitHub as follows:

```r
devtools::install_github("mczyzj/pestr")
```

## Overview and Usage

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

## Example workflow

First you need to create token variable, use your eppo token from EPPO Data Services
```r
create_eppo_token('<<your token>>')
```

Than:

* on LINUX: use function  to automatically download and unizp SQLite db from EPPO
```r
eppo_database_download()
```

* on Windows: download SQLite db using this link https://data.eppo.int/files/sqlite.zip
and extract it to working directory

Put all the names that you are looking for into a vector:

```r
pests<- c(''Xylella', Cydia packardi', 'Drosophila suzuki')
```

and connect to database.

```r
eppo_SQLite <- eppo_database_connect()
```

Get pest names, in result you will have list containing 4 tables: df with names that are present in EPPO, df with names that are not present  df with preferred names and eppo codes, df with all associated names to eppocode from third df. It is necessary to run it before other 'tabletools' functions since it extracts eppocodes that are used later by other functions.

```r
pests_names <- eppo_names_tables(pests, eppo_SQLite)
```

Get pest categorization - as result you will get list with two elements:  first is a list of tables for each eppocode one categorisation table, second element is single df with categorization for each eppocode condensed to single cell.

```r
pests_cat <- eppo_tabletools_cat(pests_names, eppo_token)
```

Get pest hosts as a result you get two tables: first is long table with all data  for all pests combined. In second hosts are combined into single cell for each eppocode

```r
pests_hosts <- eppo_tabletools_hosts(pests_names, eppo_token)
```

Get host's pests as a result you get two tables: first is long table with all data for all hosts combined. In second pests are combined into single cell for each eppocode

```r
pests_hosts <- eppo_tabletools_pests(hosts_names, eppo_token)
```

Get taxonomy. Also list with two elements. First is a list of taxonomy tables for each pest; second is table with 'main category' of each eppocode 

```r
pests_taxo <- eppo_tabletools_taxo(pests_names, eppo_token)
```

Get distributions of hosts. As a result a two element list. First one contains df of distribution for each eppocode, second contains single cell of distribution for each eppocode.

```r
pest_distri <- eppo_tabletools_distri(pests_names)
```

Whole condensed table in one shot:

```r
eppo_fulltable <- eppo_table_full(pests, eppo_SQLite, eppo_token)
```

which you can easily save as csv and use in a spreadsheet:

```r
write.csv(eppo_fulltable, 'eppo_fulltable.csv')
```

TODO:

* Code comments
* Encrypt token to make more test using TravisCI
* Update sample SQLite db
* Add Vignettes
* Internationalization
* Connection to pestrPRA
