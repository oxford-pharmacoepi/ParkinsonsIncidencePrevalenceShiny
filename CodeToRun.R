### Necessary only for package dependencies
renv::activate()
renv::restore()

library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)
library(quarto)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(ggalt)

# Combine result sets
# this will combine the results in the networkResults directory
# saving them in the shiny/data folder
source(here("mergeResults.R"))

# launch shiny
shiny::runApp(here("shiny"))
