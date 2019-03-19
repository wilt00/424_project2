library(shiny)

# source("plot.R")
# source("map.R")
# source("heatmap.R")

server <- shinyServer(function(input, output, session) {
  observeEvent(input$showAQIButton, {
    updateTabsetPanel(session, "tabset",
                      selected = "AQITab")
  })

  observeEvent(input$showPollutantsButton, {
    updateTabsetPanel(session, "tabset",
                      selected = "pollutantTab")
  })

  observeEvent(input$showMapButton, {
    updateTabsetPanel(session, "tabset",
                      selected = "mapTab")
  })

  observeEvent(input$showTempButton, {
    updateTabsetPanel(session, "tabset",
                      selected = "tempTab")
  })

  observe({
    countyList = getCounties(input$selState)
    # Update list of counties on state changed
    updateSelectInput(session,
                      "selCounty",
                      choices = countyList,
                      selected = head(countyList, 2))
  })

  output$aqiPie <- renderPlot({
    aqi_pie(input$selYear, input$selState, input$selCounty)
  })
  output$aqiBar <- renderPlot({
    aqi_bar(input$selYear, input$selState, input$selCounty)
  })
  output$aqiTable <- shiny::renderDataTable({
    aqi_table(input$selYear, input$selState, input$selCounty)
  }, options = list(pageLength = 6, dom = 't'))

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
    print(input$selYear)
    print(input$selState)
    print(input$selCounty)
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
  output$lineDailyAQI <- renderPlotly({
    ggplotly(daily_aqi_line(input$selYear,input$selState, input$selCounty))
  })
  output$tableAQI <- shiny::renderDataTable({
    write("tableAQI", stderr())
    table_month_AQI(input$selYear,input$selState, input$selCounty)
  },options = list(
    columnDefs = list(list(className= 'dt-center', targets=0:6)),
    pageLength = 12,
    searching = FALSE,
    lengthChange = FALSE,
    rownames= TRUE)
  )
  output$stackedChartAQI <- renderPlot({
    write("chartAQI", stderr())
    stackedBarChart(input$selYear,input$selState, input$selCounty)
  })

  output$multiMap <- renderPlot({
    write(input$mapType, stderr())
    write(input$selYear, stderr())
    write(input$numCounties, stderr())
    (worstCountiesMap(input$mapType, input$selYear, input$numCounties))
  })
  # output$heatmap <- renderLeaflet({
  #   if (input$mapType == "AQI") return()
  #   (pollutantHeatmap(input$mapType, input$heatmapDay))
  # })

  # About dialog
  observeEvent(input$showAboutModal, {
    showModal(
      modalDialog(
        title = "About this Page",
        p("Author: Dylan Vo, Wilfried Bedu, Will Toher"),
        p("Data Source: United States Environmental Protection Agency"),
        p("Libraries Used: "),
        p("- Shiny - Presentation"),
        p("- ggplot2 - Plotting"),
        p("- Dplyr - Data grouping"),
        p("- Purrr - Functional mapping between vectors"),
        p("- Leaflet - Mapping"),
        p("Color Palette adapted from graphiq.com"),
        easyClose = TRUE
      )
    )
  })
})

server

