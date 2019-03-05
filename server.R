
library(shiny);

shinyServer(function(input, output, session) {
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
  output$lineDailyAQI <- renderPlot({
    daily_aqi_line(input$selState, input$selCounty)
  })
  output$tableAQI <- shiny::renderDataTable({
    table_month_AQI(input$selState, input$selCounty)},
    options = list(
    pageLength = 12,
    searching = FALSE,
    lengthChange = FALSE,
    rownames= FALSE)
  )
  output$stackedChartAQI <- renderPlot({
    stackedBarChart(input$selState, input$selCounty)
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
})
