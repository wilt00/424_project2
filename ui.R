library(leaflet)
library(shinydashboard)

UI.R <- TRUE

# if(!exists("DATASOURCE.R")) source("dataSource.R")
# source("dataSource.R")


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

polutantTab <- tabPanel(
  "Pollutants",
  value = "polutantTab",
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
  value = "tempTab",
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
                 min = 1990,
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
        column(3,
               actionButton("showAQIButton", "AQI"),
               actionButton("showPolutantsButton","Polutants"),
               actionButton("showMapButton", "Map"),
               actionButton("showTempButton", "Temp")
        )
      )
    )

    ui


