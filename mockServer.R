library(shiny)

MOCKSERVER.R <- TRUE

# if(!exists("UI.R")) source("ui.R")
# if(!exists("SERVER.R")) source("server.R")

source("dataSource.R")

source("plot.R")
source("map.R")
source("heatmap.R")
source("charts.R")

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
