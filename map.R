library(ggplot2)
library(ggmap)

library(dplyr)
library(leaflet)
library(rgdal)
source("dataSource.R")

getCol <- function(mapType) {
  switch(
    mapType,
    "AQI" = "pctBadDays",
    "Ozone" = "Days.Ozone",
    "SO2" = "Days.SO2",
    "NO2" = "Days.NO2",
    "CO" = "Days.CO",
    "PM2.5" = "Days.PM2.5",
    "PM10" = "Days.PM10"
  )
}

worstCountiesMap <- function(mapType, selectedYear, numDisplayed) {
  # TODO: filter on year
  datasource <- aabc

  # If numDisplayed < 0, show all rows
  # Otherwise, ensure we don't try to display more rows than exist
  n <- ifelse(numDisplayed < 0,
              nrow(datasource),
              min(numDisplayed, nrow(datasource)))

  mapDataCol <- getCol(mapType)

  # with(aqiavg, order(mapDataCol)) gets indices of rows in datasource,
  # ordered by mapDataCol, descending
  # Then we select the first n of those indices,
  # And and get those rows of datasource, in that order
  maxVals <- datasource[order(datasource[[mapDataCol]], decreasing = TRUE)[0:n],]

  # Use inner_join from dplyr so that order of rows is preserved
  maxValsCounties <-
    inner_join(counties, maxVals, by = c("region", "subregion"))

  ggplot(data = map_data_states,
         mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(color = "black", fill = "gray") +
    geom_polygon(data = maxValsCounties,
                 aes_string(fill = mapDataCol),
                 color = "white") +
    coord_fixed(1.3) +
    guides(fill = FALSE) +
    theme_nothing()
}
