library(mapdata)
#repository of shared datasources and functions
#all read values should be done here

#######################
##General Shared data##
#######################
map_data_states <- map_data("state")
states <- sapply(unique(map_data_states$region), as.character)
counties <- map_data("county")

#returns countie on a given state
getCounties <- function(selectedState) {
  counties_by_state <- subset(counties, counties$region == selectedState ,select = (subregion))
  return(sapply(unique(counties_by_state$subregion), as.character))
}

#function to convert month number to a name
getMonth <- function(month){
  if(month=='01'){
    return("January")
  }else if(month=='02'){
    return("February")
  }else if(month=='03'){
    return("March")
  }else if(month=='04'){
    return("April")
  }else if(month=='05'){
    return("May")
  }else if(month=='06'){
    return("June")
  }else if(month=='07'){
    return("July")
  }else if(month=='08'){
    return("August")
  }else if(month=='09'){
    return("September")
  }else if(month=='10'){
    return("October")
  }else if(month=='11'){
    return("November")
  }else{
    return("December")
  }
}




########################
##Daily AQI BY Country##
########################

dabc <- read.csv("daily_aqi_by_county_2018.csv")


#########################
##Annual AQI By Country##
#########################
# Read in Average AQI By County info from files
#aabc_files <- list.files('./aabc', full.names=TRUE)
#aabc_data <- lapply(aabc_files, read.csv)
#aabc <- do.call(rbind, aabc_data)
aabc <- read.csv("./aabc/annual_aqi_by_county_2018.csv")


##############
## AQI_Sites##
##############

# Read in listing of test site locations
sites <- read.csv("aqs_sites.csv")[,c("Latitude", "Longitude", "State.Name", "County.Name")]
sites <- subset(sites, Latitude != 0.0 & Longitude != 0.0)

