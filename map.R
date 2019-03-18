library(ggplot2)
library(ggmap)
library(dplyr)
library(leaflet)
library(rgdal)

MAP.R <- TRUE

# if(!exists("DATASOURCE.R")) source("dataSource.R")
# source("dataSource.R")


worstCountiesMap <- function(mapType, selectedYear, numDisplayed) {

  datasource <- aabc %>% filter(Year==selectedYear)

  # If numDisplayed < 0, show all rows
  # Otherwise, ensure we don't try to display more rows than exist
  n <- ifelse(numDisplayed < 0,
              nrow(datasource),
              min(numDisplayed, nrow(datasource)))

  mapDataCol <- switch(
    mapType,
    "AQI" = "pctBadDays",
    "Ozone" = "Days.Ozone",
    "SO2" = "Days.SO2",
    "NO2" = "Days.NO2",
    "CO" = "Days.CO",
    "PM2.5" = "Days.PM2.5",
    "PM10" = "Days.PM10"
  )

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
                 color = "white", size=0.2) +
    coord_fixed(1.3) +
    guides(fill=guide_colorbar(title=paste(mapType, "Concentration"), color=colorbar)) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}
