library(leaflet)
library(shinydashboard)
library(plotly)

AQITab <- tabPanel(
  "AQI",
  value = "AQITab",
  splitLayout(
    tags$div(
      h3("Air Quality by Day")
    ),
    plotOutput("aqiPie"),
    dataTableOutput("aqiTable"),
    plotOutput("aqiBar")
  ),
  #TODO find out what this is and where titles should be
  fluidRow(
              h3("County AQI Over Time"),
              plotOutput("aqiLine")
  )
)

pollutantTab <- tabPanel(
  "Pollutants",
  value = "pollutantTab",
  fluidRow(
    column(
      2,
      h3("Selected Region"),
      leafletOutput("countyMap")
    ),
    column(
      5,
      h3("Majority Daily Pollutant Proportion Over Time For County"),
      plotOutput("pollutantLine")
    ),
    column(
      5,
      h3("Number of Days with Majority Pollutant"),
      plotOutput("pollutantBar")
    )
  ),
  h2("Proportion of Days with Majority Pollutant"),
  splitLayout(
    tags$div(
      h4("Carbon Monoxide"),
      plotOutput("coPie")
    ),
    tags$div(
      h4("Nitrogen Dioxide"),
      plotOutput("no2Pie")
    ),
    tags$div(
      h4("Ozone"),
      plotOutput("ozonePie")
    ),
    tags$div(
      h4("2.5 Micron Particulates"),
      plotOutput("pm25Pie")
    ),
    tags$div(
      h4("10 Micron Particulates"),
      plotOutput("pm10Pie")
    ),
    tags$div(
      h4("Sulfur Dioxide"),
      plotOutput("so2Pie")
    )
  )
)

mapTab <- tabPanel(
  "Map",
  value = "mapTab",
  splitLayout(
    shiny::dataTableOutput("pollutantTable"),
    plotOutput("multiMap")
    # leafletOutput("heatmap")
  ),
  sliderInput(
    "numCounties",
    label="numCounties",
    min = 1,
    max = 500,
    value = 100
  ),
  selectInput(
    "mapType",
    "Map: ",
    c("AQI", "CO", "NO2", "SO2", "Ozone", "PM2.5", "PM10"),
    selected = "AQI"
  ),
  # dateInput(
  #   "heatmapDay",
  #   "Day (2018): ",
  #   value="2018-01-01",
  #   min="2018-01-01",
  #   max="2018-12-31"
  # )
)

dailyTab <- tabPanel(
  "Daily",
  value = "dailyTab",
  splitLayout(
    plotlyOutput("lineDailyAQI"),
    dataTableOutput("tableAQI"),
    plotOutput("stackedChartAQI")
  )
)

hourlyTab <- tabPanel(
  "Hourly Data",
  value = "hourlyTab",
  splitLayout(
    plotlyOutput("hourlyPollutants")
  ),
  splitLayout(
    dateInput("dateInput",
              label = "Select Month and Day to view: ",
              value = "2018-01-01", min = "2018-01-01",
              max = "2018-12-01", format = "yyyy-mm-dd",
              weekstart = 0)
  )
)

body <- dashboardBody(
    tabsetPanel(
      id = "tabset",
      type = "tabs",
      AQITab,
      pollutantTab,
      mapTab,
      dailyTab,
      hourlyTab
    )
)


    ui <- fluidPage(
      fluidRow(
        body
      ),
      fluidRow(
        column(3,
               sliderInput(
                 "selYear",
                 label = h3("Year: "),
                 min = 1990,
                 max = 2018,
                 value = 1,
                 sep = ""
               )
        ),
        column(3,
               selectInput("selState", "State: ", visualStates, selected = "Illinois"),
               selectInput("selCounty", "County: ", getCounties("Illinois")),
               actionButton("showAboutModal", "About"),
               draggable = TRUE
        ),
        column(3,
               actionButton("showAQIButton", "AQI"),
               actionButton("showPolutantsButton","Polutants"),
               actionButton("showMapButton", "Map"),
               actionButton("showHourlyButton", "Hourly"),
               actionButton("showDailyButton", "Daily")
        )
      )
    )

    ui
