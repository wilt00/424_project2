library(ggplot2)
library(dplyr)
library(purrr)
library(leaflet)
library(lubridate)
library(reshape2)
library(ggrepel)
library(plotly)
library(DT)
library(reshape)
library(feather)
source("dataSource.R")
hr_oz_2018 <- read_feather("hourly_44201_2018.feather")
hr_so2_2018 <- read_feather("hourly_42401_2018.feather")
hr_pm2_2018 <- read_feather("hourly_88101_2018.feather")
hr_pm1_2018 <- read_feather("hourly_81102_2018.feather")
hr_no2_2018 <- read_feather("hourly_42602_2018.feather")
hr_co_2018 <- read_feather("hourly_42101_2018.feather")
hr_TEMP_2018 <- read_feather("hourly_TEMP_2018.feather")
allDailyAQI <- read_feather("daily_aqi_by_county_.feather")
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
#table aqi in a Dataframe
table_month_AQI <-function(selectedYear,selectedState,selectedCounty){
  region <- subset(allDailyAQI, Year==selectedYear & tolower(State.Name) == tolower(selectedState) &  tolower(county.Name) == tolower(selectedCounty))
  barChart <- data.frame(Month = character(),
                         Good = numeric(),
                         Moderate = numeric(),
                         UnhealthyforSensitiveGroups =numeric(),
                         VeryUnhealthy = numeric(),
                         Unhealthy = numeric(),
                         Hazardous = numeric())
  months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  for(month in months){
    monthinLoop <- subset(region,format.Date(Date,"%m")== month)
    name <- getMonth(month)
    newRow <- data.frame(Month = name,
                         Good = nrow(subset(monthinLoop, Category=="Good")),
                         Moderate = nrow(subset(monthinLoop, Category=="Moderate")),
                         UnhealthyforSensitiveGroups =nrow(subset(monthinLoop, Category=="Unhealthy for Sensitive Groups")),
                         VeryUnhealthy= nrow(subset(monthinLoop, Category=="Very Unhealthy")),
                         Unhealthy= nrow(subset(monthinLoop, Category=="Unhealthy")),
                         Hazardous=nrow(subset(monthinLoop, Category=="Hazardous")))
    barChart<- rbind(barChart,newRow)
  }
  df <- data.frame(barChart)
  return(df)
}

#this is the line chart for the daily aqi

daily_aqi_line <- function(selectedYear,selectedState, selectedCounty){
  region <- subset(allDailyAQI, Year==selectedYear & tolower(State.Name) == tolower(selectedState) &  tolower(county.Name) == tolower(selectedCounty))
  region[order(as.Date(region$Date, format="%y-%m-%d")),]
  ggplot(region,aes(x=Date,y=AQI, group=1, label=Defining.Parameter)) + geom_line()+geom_point(color='red') +
             theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) + xlab("Days From January to December")
}

#this is the stacked barchart took the data and sorted it by county and 
stackedBarChart <- function(selectedYear,selectedState,selectedCounty){
  region <- subset(allDailyAQI, Year==selectedYear & tolower(State.Name) == tolower(selectedState) &  tolower(county.Name) == tolower(selectedCounty))
  barChart <- data.frame(Month = character(),
                         Good = numeric(),
                         Moderate = numeric(),
                         UnhealthyforSensitiveGroups =numeric(),
                         VeryUnhealthy = numeric(),
                         Unhealthy = numeric(),
                         Hazardous = numeric())
  months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  for(month in months){
    monthinLoop <- subset(region,format.Date(Date,"%m")== month)
    name <- getMonth(month)
    newRow <- data.frame(Month = name,
                         Good = nrow(subset(monthinLoop, Category=="Good")),
                         Moderate = nrow(subset(monthinLoop, Category=="Moderate")),
                         UnhealthyforSensitiveGroups =nrow(subset(monthinLoop, Category=="Unhealthy for Sensitive Groups")),
                         VeryUnhealthy= nrow(subset(monthinLoop, Category=="Very Unhealthy")),
                         Unhealthy= nrow(subset(monthinLoop, Category=="Unhealthy")),
                         Hazardous=nrow(subset(monthinLoop, Category=="Hazardous")))
    barChart<- rbind(barChart,newRow)
  }
  dat <- melt(barChart,id.vars="Month")
  colnames(dat)[2]<-"Quality"
  ggplot(dat,aes(x=Month, y=value,fill=Quality)) + geom_bar(stat="identity") + xlab("Month")+ylab("Days")
  
}

#this is the hourly aqi line chart i Divided all the data up and filtered it to only get the average of each hour
#then combined them all and put them onto a line graph with ggplot. I send that ggplot to a plotyly graph to make it more interactive
hourly_aqi_line <- function(selectedState, selectedCounty, selectedDate){
  #hrwind <-hourly_WIND_2018 %>%
  #  dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hroz <- hr_oz_2018 %>%
    dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hroz$Parameter.Name <- "Ozone"
  hrso <- hr_so2_2018 %>%
    dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hrso$Parameter.Name <- "SO2"
  hrco <- hr_co_2018 %>%
    dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hrco$Parameter.Name <- "CO"
  hrno <- hr_no2_2018 %>%
    dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hrno$Parameter.Name <- "NO2"
  hrp2 <- hr_pm2_2018 %>%
    dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hrp2$Parameter.Name <- "PM 2.5"
  hrp1 <- hr_pm1_2018 %>%
    dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hrp1$Parameter.Name <- "PM 10"
  hrtemp <- hr_TEMP_2018 %>%
    dplyr::filter(tolower(State.Name) ==tolower(selectedState) & tolower(County.Name)==tolower(selectedCounty)& Date.Local == selectedDate )
  hrtemp$Parameter.Name <- "Temperature"
  hrtemp$val <- hrtemp$Sample.Measurement
  hrtemp$Sample.Measurement <-NULL
  
  allData <- rbind(hrco,hrno,hroz,hrp1,hrp2,hrso)
  
  ggplot(allData,aes(x=Time.Local,y=val,,group=1, color=Parameter.Name)) + geom_line() + xlab("Time") + ylab("Values")
  
}

