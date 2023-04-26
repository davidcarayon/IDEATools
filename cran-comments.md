## Test environments
* local OS X install, R 4.2.1, Apple silicon arm64 build
* Ubuntu Linux 20.04.1 LTS (on R-hub), R-release, GCC
* Fedora Linux (on R-hub) R-devel, clang, gfortan
* Windows Server 2022 (on R-hub), R-devel, 64 bit

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
'lastMiKTeXException'

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Downstream dependencies
There are currently no downstream dependencies for this package
