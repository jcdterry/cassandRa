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

## Improvements following CRAN feedback

- if there are references describing the methods in your package, please add

No citable references yet, but if/when the method is published I will add to the description in future version. 

- Please add more small executable examples in your Rd-files.

I have added examples to 3 more functions. All user-facing functions now have examples in the function documentation (as well as the vignette). 

- Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace. That is not allow by CRAN policies. Please only write/save files if the user has specified a directory. In your examples/vignettes/tests you can write to tempdir().

Apologies. I thought saving only on user request would be sufficent to fulfill CRAN policies. I have removed the option to save results  automatically. 

- Please replace cat() by message() or warning() in your functions (messages and warnings can be suppressed if needed).

Done. Apologies for the oversight!

