
<!-- README.md is generated from README.Rmd. Please edit that file -->
sensitisation
=============

R Package for the Analysis of Questionnaire Data from Routine Sensitisation and Awareness Creation Activities

How to install
--------------

In the R console simply run

``` r
devtools::install_github("NESREA/sensitisation")
```

This will download the package as well as any others that may be required for it to function. **New users should note that it may be necessary to install the `devtools` package as a first step with `install.packages("devtools")`!**

How to Use
----------

This package launches a *[Shiny](https://shiny.rstudio.com/)* application, which is useful for interactive exploratory data analysis. To view the app, simply run this line in the R console

``` r
sensitisation::display_charts()
```

The user will be prompted with a dialog to select the file containing the data. **For now, only Comma Separated Values (CSV) files are supported.**

Special notice
--------------

At the moment, the package is designed for in-house datasets with specific fields. Knowledgeable users are however welcome to use the source code to develop their own apps.

If there are problems with using the package or ideas for improvements, kindly submit an *[issue](https://github.com/NESREA/sensitisation/issues/new)*.
