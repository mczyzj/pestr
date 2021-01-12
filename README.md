# pestr

<img src="https://raw.githubusercontent.com/mczyzj/pestr/master/inst/figures/pestr-hex_center.png" width="250px" align= "right" />

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.com/mczyzj/pestr.svg?branch=master)](https://travis-ci.com/mczyzj/pestr)
[![R-CMD-check](https://github.com/mczyzj/pestr/workflows/R-CMD-check/badge.svg)](https://github.com/mczyzj/pestr/actions)
[![codecov](https://codecov.io/gh/mczyzj/pestr/branch/master/graph/badge.svg)](https://codecov.io/gh/mczyzj/pestr)
  <!-- badges: end -->


Functions included in this package allows users to painlessly connect to and extract data from **EPPO Data Services** / **EPPO Global Database**. Before you start using it you should register on: [EPPO Data Services](https://data.eppo.int/) and obtain your token to access **REST API**.

## Installation

You can install and use `pestr` development version from GitHub as follows:

```r
devtools::install_github("mczyzj/pestr")
```

## Overview and Usage

Package include function that allow you to download *SQLite* database `eppo_database_download` (archive around 12 MB, after extraction around 45 MB). The database is needed for extracting eppocodes that are used in other functions from this package. Function included in `eppo_tabletools_` family return both:

* table of raw results in so called *long format* (machine friendly) 
* processed, compact table, that contain all information in one row per one pest.

Before using functions that connect to REST API (hosts, categorization, taxonomy and pests) you should execute `create_eppo_token` function with string argument equal to your personal EPPO Data Services token. This function creates global variable `eppo_token` which should be parsed as an argument to functions that require `token` argument.

`eppo_table_full` allow to execute all the functions and return compact table with information on names, hosts, categorization, distribution and taxonomy -- one row per one pest.

Feel free to contribute to this package and report issues via *GitHub* or *email*.

## Example workflow

First you need to create token variable, use your *token* from **EPPO Data Services**.
```r
create_eppo_token('<<your token>>')
```

Than:

* on **LINUX**: use function  to automatically download and unizp *SQLite* db from **EPPO**
```r
eppo_database_download()
```

* on **Windows**: download *SQLite* db using: 
```r
eppo_database_download() #by default it downloads to working directory
#you can override this behaviour filepath argument
```
and extract the file manually to project working directory.

Put all the names that you are looking for into a vector, e.g.:
```r
pests <- c(''Xylella', Cydia packardi', 'Drosophila suzuki')
```

and make connection to database.
```r
eppo_SQLite <- eppo_database_connect()
```

### pests names

Get pest names using:

```r
pests_names_tables <- eppo_names_tables(pests, eppo_SQLite)
```

in result you will have list containing 4 tables: 

* `data frame` with names that are present in **EPPO Data Services**;
* `data frame` with names that are not present **EPPO Data Services**;
* `data frame` with preferred names and eppo codes **EPPO Data Services**;
* `data frame` with all associated names to eppocode from third `data frame`. 

You might parse results of this function directly to `eppo_tabletools_` to obtain data. Other way is to use *raw* eppocodes as argument (*this workflow is explained in Vignettes*).

### pests categorization

Using:

```r
pests_cat <- eppo_tabletools_cat(pests_names_tables, eppo_token)
```

you will get as result you will get list with two elements:  

* `data frame` with categorization tables for each eppocode in long format;
* a single `data frame` with categorization for each eppocode condensed into a single cell.

### pest hosts

```r
pests_hosts <- eppo_tabletools_hosts(pests_names_tables, eppo_token)
```

result with two tables: 

* first is a `data frame` in long format with all data for all pests; 
* second is a `data frame` where hosts are combined into single cell for each eppocode.

### pests taxonomy

To get taxonomy use: 

```r
pests_taxo <- eppo_tabletools_taxo(pests_names_tables, eppo_token)
```

This function results are a list of two `data frames`:

* first is a long format table; 
* second is table with 'main category' of each eppocode.

### pest distribution

The function extracting distribution from **EPPO Global Database** does not need `eppo_token`. It can be called like:

```r
pest_distri <- eppo_tabletools_distri(pests_names_tables)
```

The result is a two element list:

* first one contains `data frame` of distribution in long format;
* second contains single cell of distribution for each eppocode.

### pest names, categorization, distribution, taxonomy and hosts in one shot
Whole condensed table in one shot:

```r
eppo_fulltable <- eppo_table_full(pests, eppo_SQLite, eppo_token)
```

which you can easily save as csv and use in a spreadsheet:

```r
write.csv(eppo_fulltable, 'eppo_fulltable.csv')
```

### hosts pests

Since the **EPPO Data Services** provides information on pest of particular host, you can easily access information with:

```r
hosts <- c("Abies alba", "Triticum")

hosts_names_tables <- eppo_names_tables(pests, eppo_SQLite)
hosts_pests <- eppo_tabletools_taxo(hosts_names_tables, eppo_token)
```

TODO:

* Internationalization
* Connection to pestrPRA
