library(leaflet)

countiesInitial <- getCounties("Illinois")

ui <- shinyUI(fluidPage(title = "US Air Quality, 1980-2018",
                  sidebarLayout(
                    # Sidebar panel for inputs ----
                    sidebarPanel(
                      titlePanel("US Air Quality, 1980 - 2018"),
                      sliderInput(
                        "selYear",
                        label = h3("Year: "),
                        min = 1980,
                        max = 2018,
                        value = 1,
                        sep = ""
                      ),
                      selectInput("selState", "State: ", states, selected = "illinois"),
                      selectInput("selCounty", "County: ", getCounties("illinois")),
                      actionButton("showAboutModal", "About")
                    ),
                    mainPanel(
                      tabsetPanel(
                        type = "tabs",
                        tabPanel(
                          "AQI",
                          splitLayout(h3("Air Quality by Day"),
                                      h3("")),
                          splitLayout(plotOutput("aqiPie"),
                                      dataTableOutput("aqiTable")),
                          h3("Number of Days with Air Quality"),
                          plotOutput("aqiBar"),
                          h3("County AQI Over Time"),
                          plotOutput("aqiLine")
                        ),
                        tabPanel(
                          "Pollutants",
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
                        tabPanel(
                          "Map",
                          leafletOutput("countyMap"),
                          dataTableOutput("pollutantTable"),
                          plotOutput("multiMap"),
                          sliderInput(
                            "numCounties",
                            label="numCounties",
                            # inputId = "numCounties",
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
                          leafletOutput("heatMap")
                        ),
                        tabPanel("Temp",
                                 splitLayout(
                                   plotOutput("lineDailyAQI"),
                                   plotOutput("tableAQI"),
                                   plotOutput("stackedChartAQI")
                                 ))
                      )
                    )
                  )))

ui
