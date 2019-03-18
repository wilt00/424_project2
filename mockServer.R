library(shiny)

MOCKSERVER.R <- TRUE

if(!exists("UI.R")) source("ui.R")
if(!exists("SERVER.R")) source("server.R")

shinyApp(ui = ui, server = server)
