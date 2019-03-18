library(feather)

#repository of shared datasources and functions
#all read values should be done here

capitalizeEachWord <- function(str) {
  if (identical(str, character(0))) return(str)

  str_arr <- strsplit(str, " ")[[1]]
  paste(toupper(substring(str_arr, 1, 1)),
        substring(str_arr, 2),
        sep = "",
        collapse = " ")
}

#################
##feather loads##
#################

# ozoneDF <- read_feather("feather/daily_44201_.feather")
# so2DF <- read_feather("feather/daily_42401_.feather")
# co2DF <- read_feather("feather/daily_42101_.feather")
# no2 <- read_feather("feather/daily_42602_.feather")
# PM25Mass <- read_feather("feather/daily_88101_.feather")
# PM10Mass <- read_feather("feather/daily_81102_.feather")
# wind <- read_feather("feather/daily_WIND_.feather")
# temperature <- read_feather("feather/daily_TEMP_.feather")

###############
##Hourly Data##
##############

hr_oz_2018 <- read_feather("./feather/hourly_44201_2018.feather")
hr_so2_2018 <- read_feather("./feather/hourly_42401_2018.feather")
hr_pm2_2018 <- read_feather("./feather/hourly_88101_2018.feather")
hr_pm1_2018 <- read_feather("./feather/hourly_81102_2018.feather")
hr_no2_2018 <- read_feather("./feather/hourly_42602_2018.feather")
hr_co_2018 <- read_feather("./feather/hourly_42101_2018.feather")
hr_TEMP_2018 <- read_feather("./feather/hourly_TEMP_2018.feather")

#######################
##General Shared data##
#######################
map_data_states <- ggplot2::map_data("state")
states <- sapply(unique(map_data_states$region), as.character)
counties <- ggplot2::map_data("county")

visualStates <- sapply(states, capitalizeEachWord)

#returns counties on a given state
getCounties <- function(selectedState) {
  counties_by_state <- subset(counties, counties$region == tolower(selectedState), select = (subregion))
  unique(counties_by_state$subregion) %>%
    map(as.character) %>%
    map(capitalizeEachWord)
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


#######################
##Daily AQI BY County##
#######################

allDailyAQI <- read_feather("./feather/daily_aqi_by_county_.feather")

########################
##Annual AQI By County##
########################
# Read in Average AQI By County info from files
aabc_files <- list.files('./aabc', full.names=TRUE)
aabc_data <- lapply(aabc_files, read.csv)
aabc <- do.call(rbind, aabc_data)
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

