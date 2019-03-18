library(leaflet)
library(shinydashboard)
source("plot.R")
source("map.R")


AQITab <- tabPanel(
  "AQI",
  splitLayout(
    div(
      h3("Air Quality by Day"),
      plotOutput("aqiPie")
    ),
    dataTableOutput("aqiTable")
  ),
  splitLayout(h3("Number of Days with Air Quality"),
              plotOutput("aqiBar"),
              h3("County AQI Over Time"),
              plotOutput("aqiLine")
  )
)

polutantTab <- tabPanel(
  "Pollutants",
  splitLayout(
    column(
      6,
      h3("Majority Daily Pollutant Proportion Over Time For County"),
      plotOutput("pollutantLine")
    ),
    column(
      6,
      h3("Number of Days with Majority Pollutant"),
      plotOutput("pollutantBar")
    )
  ),
  h2("Proportion of Days with Majority Pollutant"),
  splitLayout(
    leafletOutput("countyMap"),
    h4("Carbon Monoxide"),
    plotOutput("coPie"),
    h4("Nitrogen Dioxide"),
    h4("Ozone"),
    plotOutput("no2Pie"),
    h4("2.5 Micron Particulates"),
    plotOutput("ozonePie"),
    h4("10 Micron Particulates"),
    plotOutput("pm25Pie"),
    h4("10 Micron Particulates"),
    plotOutput("pm10Pie"),
    h4("Sulfur Dioxide"),
    plotOutput("so2Pie")
  )
)

mapTab <- tabPanel(
  "Map",
  value = "mapTab",
  sliderInput(
    label="numCounties",
    inputId = "num",
    min = 1,
    max = 500,
    value = 100
  ),
  splitLayout(
    dataTableOutput("pollutantTable"),
    leafletOutput("multiMap")
  ),
  selectInput(
    "mapType",
    "Map: ",
    c("AQI", "CO", "NO2", "SO2", "Ozone", "PM2.5", "PM10"),
    selected = "AQI"
  )
)

tempTab <- tabPanel(
  "Temp",
  splitLayout(
  plotOutput("lineDailyAQI"),
  plotOutput("tableAQI"),
  plotOutput("stackedChartAQI")
  )
)


body <- dashboardBody(
    tabsetPanel(
      id = "tabset",
      type = "tabs",
      AQITab,
      polutantTab,
      mapTab,
      tempTab
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
                 min = 1980,
                 max = 2018,
                 value = 1,
                 sep = ""
               )
        ),
        column(3,
               selectInput("selState", "State: ", states, selected = "Illinois"),
               selectInput("selCounty", "County: ", getCounties("illinois")),
               actionButton("showAboutModal", "About"),
               draggable = TRUE
        ),
        column(6,
               actionButton("showAQIButton", "AQI"),
               actionButton("showPolutantsButton","Polutants"),
               actionButton("showMapButton", "Map"),
               actionButton("showTempButton", "Temp")
        )
      )
    )

    ui

