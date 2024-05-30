## Test environments
* Local Windows 10 R.4.4.0
Previous version had been tested via RHub on: 
* Debian Linux, R-release, GCC 
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* macOS 10.11 El Capitan, R-release (experimental)
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs reported

## Downstream dependencies

There are currently no reverse dependencies for this package.

## Fixes following email 

Removed Lazy data and issue with package aliases raised in email. 
Fixed issue of codetools being confused by dplyr syntax by declaring null global variables.
