# IDEATools 2.0.1

* Fixed a bug for excel individual reports


# IDEATools 2.0

* New version more in line with CRAN requirements
* Major code refactoring, all functions have changed
* 5 modules (read > compute > old > plot > write) 
* 1 wrapper function (diag_idea)


# IDEATools 1.1

* Added a `NEWS.md` file to track changes to the package.
* Added a two new functions for PDF reporting : `MakeReport()` and `MakeGroupReport`
* Added a two new functions for Excel reporting : `MakeExcel()` and `MakeGroupExcel` which adds a new dependency to `{openxlsx}`
* Added a new wrapper function, `DiagIDEA()` that allows users to complete a full IDEA diagnostic with only one line of code. *NOTE : This function does not yet implement the new excel and pdf reporting functions.*
* Added a new pkgdown site
