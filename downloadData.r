# I used this supplemental file to download the CSVs.
# It is not used by the Shiny application.

years <- seq(from=1980, to=2018)
for(y in years) {
  filename = paste("annual_aqi_by_county_", toString(y), ".zip", sep="")
  url = paste("https://aqs.epa.gov/aqsweb/airdata/", filename, sep="")
  download.file(url, filename, "libcurl")
  unzip(filename)
  file.remove(filename)
}

download.file("https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip", "aqs_sites.zip")
unzip("aqs_sites.zip")
