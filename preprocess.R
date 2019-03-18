library(feather)
library(dplyr)

initLog <- function(){
  cat("type, code, message",file="preprocess.log",sep="\n")
}

logStatus <- function(type, code, message){

  cat( paste(type,code,message, sep = ", "),file="preprocess.log",sep="\n", append=TRUE)
}

downloadByYearAndName <- function(years, dataName){

  for(y in years) {
    logStatus("INFO",y, "downloading years set")
    filename <- paste(dataName, toString(y), ".zip", sep="")
    dest <-  paste("archives/",filename,sep="")

    if(!file.exists(dest)){
      logStatus("INFO",filename, "downloading file")
      url = paste("https://aqs.epa.gov/aqsweb/airdata/", filename, sep="")
      download.file(url, dest, "libcurl")
    }else{
      logStatus("INFO",filename, "file exists")
    }

    extractPath <- paste("./data/",filename,sep="")
    if(!file.exists(extractPath)){
      logStatus("INFO",filename, "unziping file")
      unzip(dest, exdir = "./data")
    }else{
      logStatus("INFO",extractPath, "file exists")
    }
  }
}


reprocessHourlyPulutants <- function(df,outName){
  newDF <- df %>%
    dplyr::select(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement) %>%
    dplyr::group_by(State.Name, Time.Local,County.Name,Date.Local) %>%
    dplyr::summarise(val = mean(Sample.Measurement))
  logStatus("INFO",outName, "writing file")
  write_feather(newDF, outName)
}


downloadHourlyPolutants <- function(){
  dataNames <- c("hourly_42602_","hourly_81102_","hourly_88101_", "hourly_44201_","hourly_42401_","hourly_42101_")
  years <-seq(from = 2018, to = 2018)
  for(dataName in dataNames ){
    logStatus("INFO",dataName, "downloading file set")
    downloadByYearAndName(years, dataName)
    fileNames <- list.files(path ="./data",pattern= paste(dataName,"*",sep=""), full.names = TRUE)
    logStatus("INFO",dataName, "binding file set")
    dataByYear <- lapply(fileNames, read.csv)
    allData <- do.call(rbind, dataByYear)
    outputName =  paste("feather/", dataName,"2018.feather", sep="")
    logStatus("INFO", dataName, "writing to feather")
    reprocessHourlyPulutants(allData,outputName)
  }
}

reprocessHourlyTemp <- function(df,outName){
  newDF <- df %>%
    dplyr::select(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement) %>%
    dplyr::group_by(State.Name, Time.Local,County.Name,Date.Local)
  logStatus("INFO",outName, "writing file")
  write_feather(newDF, outName)
}

downloadHourlyTemp <- function(){
  dataName <- "hourly_TEMP_"
  years <-seq(from = 2018, to = 2018)
  logStatus("INFO",dataName, "downloading file set")
  downloadByYearAndName(years, dataName)
  fileNames <- list.files(path ="./data",pattern= paste(dataName,"*",sep=""), full.names = TRUE)
  logStatus("INFO",dataName, "binding file set")
  dataByYear <- lapply(fileNames, read.csv)
  allData <- do.call(rbind, dataByYear)
  outputName =  paste("feather/", dataName,"2018.feather", sep="")
  logStatus("INFO", dataName, "writing to feather")
  reprocessHourlyTemp(allData,outputName)
}


reprocessHourlyWind<- function(df,outName){

  logStatus("INFO", outName, " selecting values")
  newDF <- df %>% dplyr::select(State.Name, County.Name, Date.Local, Time.Local, Parameter.Name, Sample.Measurement)
  logStatus("INFO", outName, " grouping values")
  newDF <- df %>% dplyr::group_by(State.Name, Time.Local,County.Name,Date.Local)
  logStatus("INFO", outName, " filtering  values")
  newDF <- df %>% dplyr::filter(Parameter.Name == "Wind Speed - Resultant")
  logStatus("INFO",outName, "writing file")
  write_feather(newDF, outName)
}


downloadHourlyWind <- function(){
  dataName <- "hourly_WIND_"
  years <-seq(from = 2018, to = 2018)
  logStatus("INFO",dataName, "downloading file set")
  downloadByYearAndName(years, dataName)
  fileName <- "./data/hourly_WIND_2018.csv"
  logStatus("INFO",dataName, "binding file set")
  allData <- read.csv(fileName)
  outputName =  paste("feather/", dataName,"2018.feather", sep="")
  logStatus("INFO", dataName, "writing to feather")
  reprocessHourlyWind(allData,outputName)
}



reprocessDailyAQI <- function(df,outName){
  newDF <- df %>%
    dplyr::select(State.Name, county.Name, Date, AQI, Category, Defining.Parameter)
  newDF$Year <- strtoi(substring(newDF$Date,0,4), base = 0L)
  logStatus("INFO",outName, "writing file")
  write_feather(newDF, outName)
}


downloadDailyAQI <- function(){
  dataName = "daily_aqi_by_county_"
  years <-seq(from = 1990, to = 2018)
  logStatus("INFO",dataName, "downloading file set")
  downloadByYearAndName(years, dataName)
  fileNames <- list.files(path ="./data",pattern= paste(dataName,"*",sep=""), full.names = TRUE)
  logStatus("INFO",dataName, "binding file set")
  dataByYear <- lapply(fileNames, read.csv)
  allData <- do.call(rbind, dataByYear)
  outputName =  paste("feather/", dataName,".feather", sep="")
  logStatus("INFO", dataName, "writing to feather")
  reprocessDailyAQI(allData,outputName)
}

reprocessDailyPolutant <- function(df,outName){
  newDF <- dplyr::select(df, State.Code, County.Code, X1st.Max.Value, Date.Local)
  newDF$STATE <- with(newDF, sprintf("%02d", State.Code))
  newDF$COUNTY <- with(newDF, sprintf("%03d", State.Code))
  write_feather(newDF, outName)
}

downloadDailyPolutant <- function(){
  dataNames <- c("daily_44201_", "daily_42401_", "daily_42101_", "daily_42602_", "daily_88101_", "daily_81102_")
  years <-seq(from = 2018, to = 2018)
  for(dataName in dataNames ){
    logStatus("INFO",dataName, "downloading file set")
    downloadByYearAndName(years, dataName)
    fileNames <- list.files(path ="./data",pattern= paste(dataName,"*",sep=""), full.names = TRUE)
    logStatus("INFO",dataName, "binding file set")
    dataByYear <- lapply(fileNames, read.csv)
    allData <- do.call(rbind, dataByYear)
    outputName =  paste("feather/", dataName,"2018.feather", sep="")
    logStatus("INFO", dataName, "writing to feather")
    reprocessDailyPolutant(allData,outputName)
  }
}

main <- function() {
  initLog()
  downloadDailyAQI()
  downloadHourlyPolutants()
  downloadHourlyTemp()
  downloadDailyPolutant()
  downloadHourlyWind()
}

if(!interactive()) {
  main()
}else{
  main()
}

