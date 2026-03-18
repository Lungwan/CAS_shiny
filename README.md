## How to Run the App Locally

1. Open a terminal (Mac: Terminal, Windows: Command Prompt / PowerShell / Git Bash).

2. Clone this repository:

   git clone https://github.com/Lungwan/CAS_shiny

3. Navigate into the project folder:

   cd CAS_shiny

4. Open the project in R or RStudio (recommended: open the `.Rproj` file if available).

5. Install `renv` (if not already installed):

   install.packages("renv")

6. Restore the project environment:

   renv::restore()

7. Run the Shiny app:

   shiny::runApp()
