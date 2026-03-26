## How to Run the App Locally

### Install required packages
install.packages(
  "xgboost",
  repos = "https://packagemanager.posit.co/cran/2025-05-20"
)

install.packages("remotes")

remotes::install_github("jaredhuling/personalized@v0.2.7")

install.packages("shiny")

install.packages("shinyjs")

install.packages("ggplot2")

install.packages("shinydashboard")


library(shiny)

runGitHub(repo = "CAS_shiny", username = "Lungwan", ref = "main")

