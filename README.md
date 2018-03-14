# sensitisation
R Scripts for the Analysis of Questionnaire Data from Routine Sensitisation and Awareness Creation Activities

## Instructions for Use
The easiest way to explore the data (for now) is to use the *Shiny* application.
There are a number of ways to do this:

1. **From _RStudio_**: From your File Explorer, click on `sensitisation.Rproj` file to open the project in the RStudio IDE. Then on the main menu, go to *File > Open File...* and in the dialog select `app.R`. Once it is open, you will see a green arrow with *Run App* written beside it. Click it to run the application.
2. **From plain _R_**: Use `setwd()` to navigate to the project directory and simply type `runApp()` and hit ENTER.
3. **From the shell**: Navigate to the project directory and type `Rscript -e 'shiny::runApp(launch.browser = TRUE)'`

## Note for R novices
There are a number of R packages that would have to be installed for this application to work such as *shiny*, *ggplot2* and *dplyr*. To fix this, run `install.packages()` (internet connection required). For example, to install *shiny*, one would run
```
install.packages("shiny")
```

Any problems encountered with using this repository should be reported via *[Issues](https://github.com/NESREA/sensitisation/issues/new)*.
