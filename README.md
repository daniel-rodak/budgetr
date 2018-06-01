# Welcome to budgetr package!

[![Build Status](https://travis-ci.org/daniel-rodak/budgetr.svg?branch=master)](https://travis-ci.org/daniel-rodak/budgetr)

This is R package for managing home budget. It comes along with both frontend
and backend functionality.

## Installation

    install.packages('devtools')
    devtools::install_github('daniel-rodak/budgetr')

## Run app

    library(budgetr)
    launch()
    
### Run app from desktop - Windows

1. Make sure that you have added `bin` folder of your R installation (i.e. `C:\Program Files\R\R-3.4.4\bin` to your PATH envirinment variable
2. Create text file called `budgetR.bat` and insert this line into it:

        RScript -e "budgetr::launch()"
3. Double click it and the app should launch.
    
## Example budget

Example budget is located in `tests/testdata/testBudget.rds`. Clone or download repo to get it.
