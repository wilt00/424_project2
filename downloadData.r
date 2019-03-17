# I used this supplemental file to download the CSVs.
# It is not used by the Shiny application.
library(feather)

downloadByYearAndName <- function(years, dataName){
  for(y in years) {
    filename = paste(dataName, toString(y), ".zip", sep="")
    dest =  paste("archives/",filename,sep="")
    if(!file.exists(dest)){
      url = paste("https://aqs.epa.gov/aqsweb/airdata/", filename, sep="")
      download.file(url, dest, "libcurl")
      unzip(dest, exdir = "./data")
    }
  }
}

downloadAllData <- function(){
  dataNames <- c("hourly_44201_","hourly_42401_","hourly_42101_","hourly_42602_","hourly_88101_","hourly_81102_","hourly_WIND_","hourly_TEMP_")
  years <-seq(from = 1990, to = 2018)
  for(dataName in dataNames ){
    downloadByYearAndName(years, dataName)
    fileNames <- list.files(path ="./data",pattern= paste(dataName,"*",sep=""), full.names = TRUE)
    print(fileNames)
    dataByYear <- lapply(fileNames, read.csv)
    allData <- do.call(rbind, dataByYear)
    write_feather(allData, paste("feather/", dataName,".feather", sep=""))
  }
}
dir.create("data")
dir.create("archives")
dir.create("feather")

downloadAllData()

download.file("https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip", "archives/aqs_sites.zip")
unzip("archives/aqs_sites.zip", exdir = './data')
aqs_data <- read.csv("data/aqs_sites.csv")
write_feather(aqs_data, paste("feather/", "aqs_sites", ".feather", sep=""))
