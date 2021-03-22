rm(list=ls())

# MRO 3.5.3
wants <- c( "shiny",
            "shinyBS",
            "shinydashboard",
            "shinyjs",
            "shinyFiles",
            "shinythemes",
            "tidyverse",
            "xtable",
            "mrgsolve",
            "rhandsontable",
            "rmarkdown",
            "shinycssloaders",
            "PKNCA",
            "DT",
            "knitr",
            "sendmailR"
)

has   <- is.element(wants,rownames(installed.packages()))
if(any(!has)) install.packages(wants[!has])
lapply(wants[has], library, character.only = TRUE)
