# IDEATools 3.4.1
* Fixed an error in the global tree
* Re-styled some code using the tidyverse style
* Fixed a error occuring when not choosing trees as plot choice

# IDEATools 3.4.0
* Fixes to documentation
* Removed a vignette showing the dev history of IDEATools and associated images
* Droping heavy dependencies : janitor, scales, ggrepel, glue, readr, stringr and especially **dplyr** and **tidyr**
* New (light) dependency to data.table
* Fixed a bug with excel reports for properties labels
* Removed limit of 3 for group analysis
* Better dpi management for PDF reports

# IDEATools 3.3.1
* Documentation fixes for CRAN
* New logo on PDF first page
* Removed the ODT output to reduce package size
* New metadata analysis for group output
* New function for group reference excel reports and PDFs (should not be used in most cases)
* Changes to `diag_idea()` with the new "group_reference" type

# IDEATools 3.3.0
* New `runGUI()` function that loads a basic shiny app to use the package functions
* Switching from magrittr pipes to base ones (forces R > 4.1.0)
* Removed cli console printing to remove dependence
* Moved some packages to Suggest and check their presence with `rlang::check_installed()`
* Switched from `purrr::map()` to `lapply()` and `mapply()` to reduce dependencies
* New JSON examples to illustrate group analysis
* Size reduction of PDF covers

# IDEATools 3.2.1

* Better error messages for `diag_idea()`

# IDEATools 3.2.0

* Update to jsonify() function to an R object output by default
* New PDF reports with branded first pages
* Update to README asking for the `Helvetica` font
* Added facets to individual components graph
* Update to DPI argument to make it relevant

# IDEATools 3.1.1

* Added a new internal function, compile_diag()

# IDEATools 3.1.0

* Changed default font for PDF reports
* Another update to pkgdown

# IDEATools 3.0.2

* Attempt to fix a bug with colored trees for windows users
* Updated the pkgdown architecture

# IDEATools 3.0.1

* Re-introduced PPTX output format
* Fixed bugs in the "Robustesse" and "Global" trees
* Updated documentation


# IDEATools 3.0.0

* New major version
* New method used for colored trees which are now based on {ggplot2} syntax and internal reference tables for the lines and nodes coordinates. This removes the {rsvg} dependency but adds a dependency to {ggtext}.
* Changed the internal "reference" concept, from "reference_table" to "reference_list"
* Updated the pdf reports by reintroducing LaTeX and removing the {pagedown} dependency
* Updated the pdf, xlsx, docx and odt reports with the new colored trees
* Removed the html and pptx outputs
* Created new vignette about utilities such as `jsonify()`
* removed the internal "canvas" object, linked to old SVG approach for colored trees
* Fixed 4.3.3 excel IDEA calculator compatibility 

# IDEATools 2.0.8

* Added new compatibility with the 4.3.3 version of the IDEA calculator spreadcheet (json/xls/xlsx)

# IDEATools 2.0.7

* Changed the error message occuring when `read_idea()` fails in `diag_idea()`
* added a better and easier to maintain check for NA's in the output of `read_idea()`
* New categorisation rules for indicators
* New computation rules for items

# IDEATools 2.0.6

* Bug fixes and modifying `plot_idea()` as the unofficial hack `<ggplot_object> + ggsave()` does not work anymore.

# IDEATools 2.0.5

* Added new compatibility with the 4.3.1 version of the IDEA calculator spreadcheet (json/xls/xlsx)

# IDEATools 2.0.4

* Added new compatibility with the 4.3.0 version of the IDEA calculator spreadcheet (json/xls/xlsx)

# IDEATools 2.0.3

* New Feature : Frequency plot for group analysis of properties

# IDEATools 2.0.2

* Fixed a bug causing A7 to not be considered as "NC" in the properties approach (due to mis-reading of MTD_14)
* Fixed a bug causing new excel files to be considered as "old" because of MTD_15 which generated an error when divided by 100 while being imported as a character.

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
