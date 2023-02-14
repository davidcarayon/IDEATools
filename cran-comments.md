## Test environments
* local OS X install, R 4.2.1, Apple silicon arm64 build
* Ubuntu Linux 20.04.1 LTS (on R-hub), R-release, GCC
* Fedora Linux (on R-hub) R-devel, clang, gfortan
* Windows Server 2022 (on R-hub), R-devel, 64 bit

## R CMD check results
There were no ERRORs or WARNINGs.

There was 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘David Carayon <david.carayon@inrae.fr>’

This NOTE can be safely ignored as it's my first submission.

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
'lastMiKTeXException'

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

* ONLY on Fedora Linux (R-hub): checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found.

I cannot change that Tidy is not on the path, or update Tidy on the external Fedora Linux server.

## Downstream dependencies
There are currently no downstream dependencies for this package