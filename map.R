library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)

# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

states <- map_data("state")
counties <- map_data("county")

# dailyAQIByYear <- list()

dailyaqi <- read.csv("output/daily_aqi_by_county/daily_aqi_by_county_2018.csv")

aqilvls <- ordered(unique(dailyaqi$Category),
                   levels = c("Good", "Moderate", "Unhealthy",
                              "Unhealthy for Sensitive Groups",
                              "Very Unhealthy", "Hazardous"))

# Group dailyaqi by the "State.Name" and "county.Name" columns, and apply the mean function to the
# "AQI" column for each group
aqiavg <- aggregate(dailyaqi[,"AQI"],
                    by=list(dailyaqi$State.Name, dailyaqi$county.Name),
                    FUN=mean)
# Equivalent SQL for previous two lines:
# SELECT AVG(AQI) AS AvgAQI, State, County
# FROM [daily aqi table]
# WHERE year=2018
# GROUP BY State.Name, county.Name
# ORDER BY AvgAQI
# # LIMIT 100  <- maybe not

names(aqiavg) <- c("State", "County", "AQI")

# map_theme <- theme(
#   axis.text = element_blank(),
#   axis.line = element_blank(),
#   axis.ticks = element_blank(),
#   panel.border = element_blank(),
#   panel.grid = element_blank(),
#   axis.title = element_blank()
# )

# with(aqiavg, order(-x)) gets indices of rows in dataframe aqiavg, ordered by the column "x",
# descending
# Then we select the first 100 of those, and get those rows of maxaqi, in that order
maxaqi <- aqiavg[with(aqiavg, order(-AQI))[0:100],]

maxaqi$region <- with(maxaqi, tolower(State))
maxaqi$subregion <- with(maxaqi, tolower(County))

# Use inner_join from dplyr so that order of rows is preserved
maxaqicounties <- inner_join(counties, maxaqi, by=c("region","subregion"))

ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(color="black", fill="gray") +
  geom_polygon(data=maxaqicounties, aes(fill=AQI), color="white") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  theme_nothing()

