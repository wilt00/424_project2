library(mapdata)
library(feather)
#repository of shared datasources and functions
#all read values should be done here

#################
##feather loads##
#################
ozoneDF <- read_feather("feather/daily_44201_.feather")
so2DF <- read_feather("feather/daily_42401_.feather")
co2DF <- read_feather("feather/daily_42101_.feather")
no2 <- read_feather("feather/daily_42602_.feather")
PM25Mass <- read_feather("feather/daily_88101_.feather")
PM10Mass <- read_feather("feather/daily_81102_.feather")
wind <- read_feather("feather/daily_WIND_.feather")
temperature <- read_feather("feather/daily_TEMP_.feather")


#######################
##General Shared data##
#######################
map_data_states <- map_data("state")
states <- sapply(unique(map_data_states$region), as.character)
counties <- map_data("county")

#returns counties on a given state
getCounties <- function(selectedState) {
  counties_by_state <- subset(counties, counties$region == selectedState ,select = (subregion))
  return(sapply(unique(counties_by_state$subregion), as.character))
}

#function to convert month number to a name
getMonth <- function(month){
  switch(month,
         '01' = "January",
         '02' = "February",
         '03' = "March",
         '04' = "April",
         '05' = "May",
         '06' = "June",
         '07' = "July",
         '08' = "August",
         '09' = "September",
         '10' = "October",
         '11' = "November",
         '12' = "December")
}

months <- c(1,2,3,4,5,6,7,8,9,10,11,12)


########################
##Daily AQI BY Country##
########################

dabc <- read.csv("daily_aqi_by_county_2018.csv")


#########################
##Annual AQI By Country##
#########################
# Read in Average AQI By County info from files
aabc_files <- list.files('./aabc', full.names=TRUE)
aabc_data <- lapply(aabc_files, read.csv)
aabc <- do.call(rbind, aabc_data)
#aabc <- read.csv("./aabc/annual_aqi_by_county_2018.csv")
aabc$region <- with(aabc, tolower(State))
aabc$subregion <- with(aabc, tolower(County))
aabc$pctBadDays =
  with(aabc, (Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days) / Days.with.AQI)

##############
## AQI_Sites##
##############

# Read in listing of test site locations
sites <- read.csv("aqs_sites.csv")[,c("Latitude", "Longitude", "State.Name", "County.Name")]
sites <- subset(sites, Latitude != 0.0 & Longitude != 0.0)

