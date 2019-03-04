library(ggplot2)
library(dplyr)
library(purrr)
library(leaflet)
library(lubridate)
library(reshape2)
library(ggrepel)
data <- read.csv("daily_aqi_by_county_2018.csv")

states <- sapply(unique(data$State.Name), as.character)
getCounties <- function(selectedState) {
  daily_state <- subset(data, State.Name == selectedState)
  return(sapply(unique(daily_state$County.Name), as.character))
}
region <- subset(data,State.Name == "Illinois" &  county.Name== "Cook")
region <- region[,-c(4,5)]
region[order(as.Date(region$Date, format="%y-%m-%d")),]

lineGraph <- ggplot(region,aes(x=Date,y=AQI, group=1,label=region$Defining.Parameter)) + geom_line()+geom_point()


table_month_AQI <-function(selectedState,selectedCounty){
  region <- subset(data,State.Name == selectedState &  county.Name==selectedCounty)
  months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  barChart <- data.frame(Month = character(),
                         Good = numeric(),
                         Moderate = numeric(),
                         UnhealthyforSensitiveGroups =numeric(),
                         VeryUnhealthy = numeric(),
                         Unhealthy = numeric(),
                         Hazardous = numeric())
  for(month in months){
    monthinLoop = subset(region,format.Date(Date,"%m")== month)
    if(month=='01'){
      month <-"January"
    }else if(month=='02'){
      month <-"February"
    }else if(month=='03'){
      month <-"March"
    }else if(month=='04'){
      month <-"April"
    }else if(month=='05'){
      month <-"May"
    }else if(month=='06'){
      month <-"June"
    }else if(month=='07'){
      month <-"July"
    }else if(month=='08'){
      month <-"August"
    }else if(month=='09'){
      month <-"September"
    }else if(month=='10'){
      month <-"October"
    }else if(month=='11'){
      month <-"November"
    }else{
      month <-"December"
    }
    newRow <- data.frame(Month = month,
                         Good = nrow(subset(monthinLoop, Category=="Good")),
                         Moderate = nrow(subset(monthinLoop, Category=="Moderate")),
                         UnhealthyforSensitiveGroups =nrow(subset(monthinLoop, Category=="Unhealthy for Sensitive Groups")),
                         VeryUnhealthy= nrow(subset(monthinLoop, Category=="Very Unhealthy")),
                         Unhealthy= nrow(subset(monthinLoop, Category=="Unhealthy")),
                         Hazardous=nrow(subset(monthinLoop, Category=="Hazardous")))
    barChart<- rbind(barChart,newRow)
  }
  DT::renderDataTable({barChart},
                      options = list(searching = FALSE,lengthChange = FALSE,rownames= FALSE)
  )
}

daily_aqi_line <- function(selectedState, selectedCounty){
  aabc_region <- subset(aabc, State == selectedState & County == selectedCounty)


}
stackedBarChart <- function(selectedState, selectedCounty){
  region <- subset(data,State.Name == selectedState &  county.Name==selectedCounty)
  months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  barChart <- data.frame(Month = character(),
                         Good = numeric(),
                         Moderate = numeric(),
                         UnhealthyforSensitiveGroups =numeric(),
                         VeryUnhealthy = numeric(),
                         Unhealthy = numeric(),
                         Hazardous = numeric())
  for(month in months){
    monthinLoop = subset(region,format.Date(Date,"%m")== month)
    if(month=='01'){
      month <-"January"
    }else if(month=='02'){
      month <-"February"
    }else if(month=='03'){
      month <-"March"
    }else if(month=='04'){
      month <-"April"
    }else if(month=='05'){
      month <-"May"
    }else if(month=='06'){
      month <-"June"
    }else if(month=='07'){
      month <-"July"
    }else if(month=='08'){
      month <-"August"
    }else if(month=='09'){
      month <-"September"
    }else if(month=='10'){
      month <-"October"
    }else if(month=='11'){
      month <-"November"
    }else{
      month <-"December"
    }
    newRow <- data.frame(Month = month,
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
