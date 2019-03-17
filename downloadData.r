# I used this supplemental file to download the CSVs.
# It is not used by the Shiny application.


library(feather)


initLog <- function(){
  cat("type, code, message",file="downloadData.log",sep="\n")
}

logStatus <- function(type, code, message){

  cat( paste(type,code,message, sep = ", "),file="downloadData.log",sep="\n", append=TRUE)
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

downloadAllData <- function(){
  dataNames <- c("hourly_44201_","hourly_42401_","hourly_42101_","hourly_42602_","hourly_88101_","hourly_81102_","hourly_WIND_","hourly_TEMP_")
  years <-seq(from = 1990, to = 1991)
  for(dataName in dataNames ){
    logStatus("INFO",dataName, "downloading file set")
    downloadByYearAndName(years, dataName)
    fileNames <- list.files(path ="./data",pattern= paste(dataName,"*",sep=""), full.names = TRUE)
    logStatus("INFO",dataName, "binding file set")
    dataByYear <- lapply(fileNames, read.csv)
    allData <- do.call(rbind, dataByYear)
    logStatus("INFO", dataName, "writing to feather")
    write_feather(allData, paste("feather/", dataName,".feather", sep=""))
  }
}



dir.create("data")
dir.create("archives")
dir.create("feather")

initLog();
downloadAllData()

download.file("https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip", "archives/aqs_sites.zip")
unzip("archives/aqs_sites.zip", exdir = './data')
aqs_data <- read.csv("data/aqs_sites.csv")
write_feather(aqs_data, paste("feather/", "aqs_sites", ".feather", sep=""))
