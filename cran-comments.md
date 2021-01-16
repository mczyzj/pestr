## Release summary
This is a resubmission.

## Feedbacks from previous submission:

```
Please do not modifiy the .GlobalEnv by deleting objects. This is not
allowed by the CRAN policies.
```
-> Corrected -- This error came from having side-effects of one of the functions, and deleting the variable from .GlobalEnv in the test. Now functions do not alter .GlobalEnv by themselves, neither do tests.

```
Thanks, we see:

   The Title field should be in title case. Current version is:
   'Interface to Download Data on Pests And Hosts from 'EPPO''
   In title case that is:
   'Interface to Download Data on Pests and Hosts from 'EPPO''

Please fix and resubmit.
```

-> Title is now in correct title case.

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
