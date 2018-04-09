
<!-- README.md is generated from README.Rmd. Please edit that file -->
sensitisation [![Travis-CI Build Status](https://travis-ci.org/BroVic/sensitisation.svg?branch=master)](https://travis-ci.org/BroVic/sensitisation)
===================================================================================================================================================

R Package for the Analysis of Questionnaire Data from Routine Sensitisation and Awareness Creation Activities

How to install
--------------

In the R console simply run

``` r
devtools::install_github("NESREA/sensitisation")
```

New users should note that it may be necessary to first install `devtools` with `install.packages("devtools")`!

How to Use
----------

This package launches a *[Shiny](https://shiny.rstudio.com/)* application, which is useful for interactive exploratory data analysis. To view the app, simply pass the path to the **CSV** file as an argument to `display_data()`

``` r
library(sensitisation)
display_data("path/to/csv/file")
```

Special notice
--------------

This package is designed for in-house datasets containing specific fields and its functions will need to be modified prior to use on generic data.
