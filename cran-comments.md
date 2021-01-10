## Release summary
This is first submission to CRAN.

## Test environments
* local Ubuntu 18.04 install, R 4.0.3
* win-builder (devel and release)
* ubuntu 18.04.5 (on github actions), R-devel, R 4.0.3, R 3.6.3
* mac OS 10.15.7 (on github actions) R 4.0.3
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.0.3 R 3.6.3

## R CMD check results
There were no ERRORs or WARNINGs.
There was one NOTE on win-builder regarding 'checking CRAN incoming feasibility'. 

## Downstream dependencies
This is first submission, so there are no downstream dependencies.

## Tests
* Some functions that need authorization token were tested manually, and are skipped on CRAN.
* Results of web queries were mocked up and used in tests.
* There are 7 'RDS' mock ups of web query results that are used in test, in 'tests/testthat' directory and one 'eppocode.sqlite` file which is artificial sample of proper database, also used for testing functions.

## Vignettes
* There are 9 'RDS' files that are mock ups of web queries that are used to demonstrate functions output in 'vignettes' directory.

## Downloading files
* 'eppo_database_download' function by default downloads 'zip' file to working directory. This behaviour might be changed with 'filepath' argument.

## Fail gracefully
* functions that are used to download or read information from web resources give messages instead of errors on fail. The wrappers are in 'eppo_api.R' file. 'eppo_database_download' and 'check_eppo_token' functions has own implementation in function body.
