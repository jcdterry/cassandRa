## Test environments
* Local Windows 10 R.3.6.0
Via RHub: 
* Debian Linux, R-release, GCC 
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* macOS 10.11 El Capitan, R-release (experimental)
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs reported, apart from 'New Submission' note.

## Downstream dependencies

There are currently no reverse dependencies for this package.

## Improvements following CRAN feedback (2):

- please replace \dontrun{} by \donttest{} or unwrap the examples if they can be executed in less thatn 5 sec per Rd-file

Examples in documentation take longer than 5 seconds. To allow testing, I have added compact and rapid tests wrapped in \dontshow{} to all the user facing functions. On rhub these take less than 5 seconds.  


