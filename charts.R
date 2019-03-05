library(ggplot2)
library(dplyr)
library(purrr)
library(leaflet)
library(lubridate)
library(reshape2)
library(ggrepel)
library(plotly)
library(DT)
data <- read.csv("daily_aqi_by_county_2018.csv")

states <- sapply(unique(data$State.Name), as.character)

getCounties <- function(selectedState) {
  daily_state <- subset(data, State.Name == selectedState)
  return(sapply(unique(daily_state$County.Name), as.character))
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
region <- subset(data,State.Name == "Illinois" &  county.Name=="Cook")
months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
barChart <- data.frame(Month = character(),
                       Good = numeric(),
                       Moderate = numeric(),
                       UnhealthyforSensitiveGroups =numeric(),
                       VeryUnhealthy = numeric(),
                       Unhealthy = numeric(),
                       Hazardous = numeric())
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




#This should display the table
table_month_AQI <-function(selectedState,selectedCounty){
  df <- data.frame(barChart)
  rownames(df) <-df$Month
  df$Month <- NULL
  return(df)
}

#this is the line chart
daily_aqi_line <- function(selectedState, selectedCounty){
  region <- subset(data,State.Name == "Illinois" &  county.Name== "Cook")
  region[order(as.Date(region$Date, format="%y-%m-%d")),]
  line <- ggplot(region,aes(x=Date,y=AQI, group=1,label=Defining.Parameter)) + geom_line()+geom_point(color='red') +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + xlab("Days From January to December")
  return(ggplotly(line))

}


#this is the stacked barchart
stackedBarChart <- function(selectedState, selectedCounty){
  dat <- melt(barChart,id.vars="Month")
  colnames(dat)[2]<-"Quality"
  ggplot(dat,aes(x=Month, y=value,fill=Quality)) + geom_bar(stat="identity") + xlab("Month")+ylab("Days")
}
