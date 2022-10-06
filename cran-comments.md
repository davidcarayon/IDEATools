## Answers to review #1 (Submitted 2022-10-04)
Thank you very much for your valuable remarks. I hope that this new version should fix my errors.

* I added a reference to the IDEA4 method following your guidelines in the DESCRIPTION file
* I reduced the package size under 5MB as requested
* I added and documented the `\value` tags as requested

* Switched from `\dontrun{}` to `\donttest{}` when the example required some other files to work. The only `\dontrun{}` call still there is used is for a function loading a shiny app (`runGUI()`). I can't see how to avoid it and saw it on various other CRAN packages in this specific case.

* Removed the `options(warn = -1)` call
* I made sure that the only place where I change the working directory of the user, I reset it to the original one straight after code execution

## R CMD check results
There were no ERRORs or WARNINGs.

There was 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘David Carayon <david.carayon@inrae.fr>’

This NOTE can be safely ignored as it's my first submission.

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
'lastMiKTeXException'

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Downstream dependencies
There are currently no downstream dependencies for this package

## Test environments
* local OS X install, R 4.1.2
* Ubuntu Linux 20.04.1 LTS (on R-hub), R-release
* Fedora Linux (on R-hub) R-devel, clang, gfortan
* Windows (devel and release)
