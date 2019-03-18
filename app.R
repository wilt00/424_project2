library(shiny)
library(leaflet)

countiesInitial <- getCounties("Illinois")

ui <- fluidPage(
  fluidRow(
    column(1,
           # Settings column
           titlePanel("US Air Quality, 1980 - 2018"),
           sliderInput("selYear", label=h3("Year: "), min=1980, max=2018, value=1, sep=""),
           selectInput("selState", "State: ", states, selected="Illinois"),
           selectInput("selCounty", "County: ", countiesInitial, selected="Cook"),
           actionButton("showAboutModal", "About")
    ),
    column(4,
           splitLayout(
             h3("Air Quality by Day"),
             h3("")
           ),
           splitLayout(
             plotOutput("aqiPie"),
             dataTableOutput("aqiTable")
           ),
           h3("Number of Days with Air Quality"),
           plotOutput("aqiBar"),
           h3("County AQI Over Time"),
           plotOutput("aqiLine")
    ),
    column(4,
           h3("Majority Daily Pollutant Proportion Over Time For County"),
           plotOutput("pollutantLine"),
           h3("Number of Days with Majority Pollutant"),
           plotOutput("pollutantBar"),
           h2("Proportion of Days with Majority Pollutant"),
           splitLayout(
             h4("Carbon Monoxide"),
             h4("Nitrogen Dioxide"),
             h4("Ozone"),
             h4("2.5 Micron Particulates"),
             h4("10 Micron Particulates"),
             h4("Sulfur Dioxide")
           ),
           splitLayout(
             plotOutput("coPie"),
             plotOutput("no2Pie"),
             plotOutput("ozonePie"),
             plotOutput("pm25Pie"),
             plotOutput("pm10Pie"),
             plotOutput("so2Pie")
           )
    ),
    column(3,
           leafletOutput("countyMap"),
           splitLayout(
            dataTableOutput("pollutantTable")
           )
    )
  ),
  title="US Air Quality, 1980-2018"
)

pieSize <- 200

server <- function(input, output, session) {
  observe({
    # Update list of counties on state changed
    updateSelectInput(session, "selCounty", choices=getCounties(input$selState), selected = "Cook")
  })

  output$aqiPie <- renderPlot({
    aqi_pie(input$selYear, input$selState, input$selCounty)
  })
  output$aqiBar <- renderPlot({
    aqi_bar(input$selYear, input$selState, input$selCounty)
  })
  output$aqiTable <- shiny::renderDataTable({
    aqi_table(input$selYear, input$selState, input$selCounty)
  }, options=list(pageLength=6, dom = 't'))

  output$coPie <- renderPlot({
    pollutant_pie(input$selYear, input$selState, input$selCounty, "CO")
  })

  output$no2Pie <- renderPlot({
    pollutant_pie(input$selYear, input$selState, input$selCounty, "NO2")
  })

  output$ozonePie <- renderPlot({
    pollutant_pie(input$selYear, input$selState, input$selCounty, "Ozone")
  })

  output$so2Pie <- renderPlot({
    pollutant_pie(input$selYear, input$selState, input$selCounty, "SO2")
  })

  output$pm25Pie <- renderPlot({
    pollutant_pie(input$selYear, input$selState, input$selCounty, "PM2.5")
  })

  output$pm10Pie <- renderPlot({
    pollutant_pie(input$selYear, input$selState, input$selCounty, "PM10")
  })

  output$pollutantBar <- renderPlot({
    pollutant_bar(input$selYear, input$selState, input$selCounty)
  })

  output$aqiLine <- renderPlot({
    aqi_line(input$selState, input$selCounty)
  })
  output$pollutantLine <- renderPlot({
    pollutant_line(input$selState, input$selCounty)
  })
  output$pollutantTable <- shiny::renderDataTable({
    pollutant_table(input$selState, input$selCounty)
  })
  output$countyMap <- renderLeaflet({
    (mapCounty(input$selState, input$selCounty))
  })

  # About dialog
  observeEvent(input$showAboutModal, {
    showModal(modalDialog(
      title="About this Page",
      p("Author: Will Toher"),
      p("Data Source: United States Environmental Protection Agency"),
      p("Libraries Used: "),
      p("- Shiny - Presentation"),
      p("- ggplot2 - Plotting"),
      p("- Dplyr - Data grouping"),
      p("- Purrr - Functional mapping between vectors"),
      p("- Leaflet - Mapping"),
      p("Color Palette adapted from graphiq.com"),
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)
