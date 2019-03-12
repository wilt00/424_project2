library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
source("dataSource.R")

aabc$pctBadDays =
  with(aabc, (Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days) / Days.with.AQI)

worstAQICountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aabc))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aabc[with(aabc, order(-pctBadDays))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=map_data_states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="black", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=pctBadDays), color="white") +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}

worstOzoneCountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aabc))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aabc[with(aabc, order(-Days.Ozone))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=map_data_states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="black", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.Ozone), color="white") +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}

worstSO2CountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

   numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aabc))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aabc[with(aabc, order(-Days.SO2))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=map_data_states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="gray", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.SO2)) +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}

worstNO2CountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aabc))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aabc[with(aabc, order(-Days.NO2))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=map_data_states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="gray", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.NO2)) +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}

worstCOCountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aabc))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aabc[with(aabc, order(-Days.CO))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=map_data_states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="gray", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.CO)) +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}

worstPM25CountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aabc))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aabc[with(aabc, order(-Days.PM2.5))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=map_data_states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="gray", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.PM2.5)) +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}

worstPM10CountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aabc))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aabc[with(aabc, order(-Days.PM10))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=map_data_states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="gray", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.PM10)) +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}
