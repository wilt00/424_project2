library(ggplot2)
library(dplyr)
library(purrr)
library(leaflet)
library(lubridate)
library(reshape2)
library(ggrepel)
library(plotly)
library(DT)
source("dataSource.R")


region <- subset(dabc,State.Name == "Illinois" &  county.Name=="Cook")

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
  datatable(df,
            options = list(
            columnDefs = list(list(className= 'dt-center', targets=0:6)),
            pageLength = 12,
            searching = FALSE,
            lengthChange = FALSE,
            rownames= TRUE)
  )
}

#this is the line chart
daily_aqi_line <- function(selectedState, selectedCounty){
  region <- subset(dabc,State.Name == "Illinois" &  county.Name== "Cook")
  region[order(as.Date(region$Date, format="%y-%m-%d")),]
  line <- ggplot(region,aes(x=Date,y=AQI, group=1,label=Defining.Parameter)) + geom_line()+geom_point(color='red') +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + xlab("Days From January to December")
  ggplotly(line)

}


#this is the stacked barchart
stackedBarChart <- function(selectedState, selectedCounty){
  dat <- melt(barChart,id.vars="Month")
  colnames(dat)[2]<-"Quality"
  ggplot(dat,aes(x=Month, y=value,fill=Quality)) + geom_bar(stat="identity") + xlab("Month")+ylab("Days")
}
