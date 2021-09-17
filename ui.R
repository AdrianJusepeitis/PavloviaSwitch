library(shinyalert)
library(shinycssloaders)
library(tidyr)
library(shinythemes)

# Define UI ----
ui <- fluidPage(
  useShinyalert(),
  uiOutput("Ui") %>% withSpinner()

)
