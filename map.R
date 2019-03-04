library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)

# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

states <- map_data("state")
counties <- map_data("county")

# dailyAQIByYear <- list()

# dailyaqi <- read.csv("output/daily_aqi_by_county/daily_aqi_by_county_2018.csv")

# aqilvls <- ordered(unique(dailyaqi$Category),
#                    levels = c("Good", "Moderate",
#                               "Unhealthy for Sensitive Groups",
#                               "Unhealthy", "Very Unhealthy", "Hazardous"))
# Group dailyaqi by the "State.Name" and "county.Name" columns, and apply the mean function to the
# "AQI" column for each group
# aqiavg <- aggregate(dailyaqi[,"AQI"],
#                     by=list(dailyaqi$State.Name, dailyaqi$county.Name),
#                     FUN=mean)
# Equivalent SQL for previous two lines:
# SELECT AVG(AQI) AS AvgAQI, State, County
# FROM [daily aqi table]
# WHERE year=2018
# GROUP BY State.Name, county.Name
# ORDER BY AvgAQI
# # LIMIT 100  <- maybe not
# names(aqiavg) <- c("State", "County", "AQI")


aqiavg <- read.csv("./aabc/annual_aqi_by_county_2018.csv")
aqiavg$pctBadDays =
  with(aqiavg, (Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days) / Days.with.AQI)

worstAQICountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aqiavg))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aqiavg[with(aqiavg, order(-pctBadDays))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
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
  n <- min(numDisplayed, nrow(aqiavg))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aqiavg[with(aqiavg, order(-Days.Ozone))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="black", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.Ozone), color="white") +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}

worstSO2CountiesMap <- function(selectedYear, numDisplayed) {

  # TODO: filter on year

  # numDisplayed <- 100
  # Ensure we don't try to display more rows than exist in aqiavg
  n <- min(numDisplayed, nrow(aqiavg))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aqiavg[with(aqiavg, order(-Days.SO2))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
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
  n <- min(numDisplayed, nrow(aqiavg))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aqiavg[with(aqiavg, order(-Days.NO2))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
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
  n <- min(numDisplayed, nrow(aqiavg))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aqiavg[with(aqiavg, order(-Days.CO))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
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
  n <- min(numDisplayed, nrow(aqiavg))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aqiavg[with(aqiavg, order(-Days.PM2.5))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
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
  n <- min(numDisplayed, nrow(aqiavg))

  # with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
  # descending
  # Then we select the first 100 of those, and get those rows of maxaqi, in that order
  # maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]
  maxaqi <- aqiavg[with(aqiavg, order(-Days.PM10))[0:n],]

  maxaqi$region <- with(maxaqi, tolower(State))
  maxaqi$subregion <- with(maxaqi, tolower(County))

  # Use inner_join from dplyr so that order of rows is preserved
  maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

  ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
    geom_polygon(color="gray", fill="gray") +
    geom_polygon(data=maxaqicounties, aes(fill=Days.PM10)) +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing()
}
