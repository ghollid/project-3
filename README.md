# project-3

Grace Holliday  

- [Purpose](#purpose)
- [Required packages](#required-packages)
- [Code to Install Packages](#code-to-install-packages)
- [Shiny Run Github Code](#shiny-run-github-code)

## Purpose

The purpose of this app is to produce exploratory data analysis on a 
dataset containing information on the graduation status of students 
at a higher education institution.  We hope to explore what factors 
may influence a student's retention and eventual graduation from a 
University.  The user is able to produce three models that will predict
graduation or dropout status.

## Required packages

The following packages are required:

- `rmarkdown`: Create and render the markdown files
- `shiny`: Create UI/Server files
- `tidyverse`: All manner of data manipulation/figure-making activities
- `caret`: Used to create/train the various models
- `ggplot2`: Used to create exploratory plots
- `gbm`: Necessary to create boosted tree model
- `randomForest`: Necessary to create the random forest model
- `DT`: Necessary to create data table output in the UI

## Code to Install Packages

The code to install all of these packages can be found below:

```{r, echo=TRUE}
install.packages('rmarkdown')
install.packages('shiny')
install.packages('tidyverse')
install.packages('caret')
install.packages('ggplot2')
install.packages('gbm')
install.packages('randomForest')
install.packages('DT')
```

## Shiny Run Github Code

Below is the shiny::runGithub() code:

```{r, echo=TRUE}
shiny::runGitHub('ghollid/project-3')
```

