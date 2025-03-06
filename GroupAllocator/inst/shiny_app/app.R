library(shiny)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

# to run the app locally:
# cd ~/Desktop/fyp/GroupAllocator/inst/shiny_app
# R
# shiny::runApp("app.R")
