library(magrittr)

# if (!exists("DATASOURCE.R")) source("dataSource.R")
# source("dataSource.R")

## PREPROCESS
processDailyPollutant <- function(pdata) {
  pdata <- dplyr::select(pdata, State.Code, County.Code, X1st.Max.Value, Date.Local)
  pdata$STATE <- with(pdata, sprintf("%02d", State.Code))
  pdata$COUNTY <- with(pdata, sprintf("%03d", County.Code))
  return(pdata)
}

daily_oz <- processDailyPollutant(read.csv("./data/daily_44201_2018.csv"))
daily_so2 <- processDailyPollutant(read.csv("./data/daily_42401_2018.csv"))
daily_co <- processDailyPollutant(read.csv("./data/daily_42101_2018.csv"))
daily_no2 <- processDailyPollutant(read.csv("./data/daily_42602_2018.csv"))
daily_pm25 <- processDailyPollutant(read.csv("./data/daily_88101_2018.csv"))
daily_pm10 <- processDailyPollutant(read.csv("./data/daily_81102_2018.csv"))


getStateName <- function(stateCode) {
  if (class(stateCode) == "character") {
    stateCode <- strtoi(stateCode)
  }
  lname <-
    maps::state.fips[which(maps::state.fips$fips == stateCode), "polyname"]
  if (identical(lname, character(0))) {
    return("??")
  }
  capitalizeEachWord(lname)
}

countiesJ <- rgdal::readOGR("gz_2010_us_050_00_20m.json")
countiesJ@data$STATE <- sapply(countiesJ@data$STATE, as.character)
countiesJ@data$COUNTY <- sapply(countiesJ@data$COUNTY, as.character)
countiesJ@data$StateName <-
  sapply(countiesJ@data$STATE, getStateName)
countiesDataBkp <- data.frame(countiesJ@data)


pollutantHeatmap <- function(mapType, month, day) {
  data <- switch(mapType,
                 "Ozone" = daily_oz,
                 "SO2" = daily_so2,
                 "CO" = daily_co,
                 "NO2" = daily_no2,
                 "PM2.5" = daily_pm25,
                 "PM10" = daily_pm10)

  newData <- data.frame(countiesDataBkp)
  countiesJ@data <-
    dplyr::full_join(newData, data, by = c("STATE", "COUNTY"))

  labels <- with(
    countiesJ@data,
    sprintf(
      "<strong>%s - %s</strong><br/>%s: %f",
      StateName,
      NAME,
      mapType,
      val
    )
  ) %>%
    lapply(htmltools::HTML)

  pal <- leaflet::colorBin("YlOrRd", domain = countiesJ@data$val)

  lbase <- leaflet::leaflet(countiesJ) %>%
    leaflet::setView(-96, 37.8, 4) %>%
    leaflet::addPolygons(
      fillColor = ~ pal(val),
      weight = 2,
      opacity = 0.4,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = ~ val,
      opacity = 0.7,
      position = "bottomleft"
    )

  lbase
}







