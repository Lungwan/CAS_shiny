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

<img width="1675" height="787" alt="Screenshot 2026-03-26 at 20 53 47" src="https://github.com/user-attachments/assets/f141af21-3a25-45cf-9f9b-90150d9d81e9" />
