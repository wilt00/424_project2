library(ggplot2)
library(dplyr)
library(purrr)
library(leaflet)
source("dataSource.R")

# Define function to retrieve unique counties for a given state
getCounties <- function(selectedState) {
  aabc_state <- subset(aabc, State == selectedState)
  return(sapply(unique(aabc_state$County), as.character))
}


# Create column representing state and county name, to eliminate impact of duplicate county names
sites$StateCounty <- with(sites, paste0(State.Name, "#", County.Name))

# Group and summarize latitude and longitude of measurement stations by county
locations <- sites %>%
  group_by(StateCounty) %>%
  summarize(Latitude = mean(Latitude), Longitude = mean(Longitude))

# List names of columns with AQI info
AqiColumnNames <- c("Good.Days", "Moderate.Days", "Unhealthy.for.Sensitive.Groups.Days", "Unhealthy.Days", "Very.Unhealthy.Days", "Hazardous.Days")
AqiLineColumns <- c("Year", "Max.AQI", "X90th.Percentile.AQI", "Median.AQI")

# Define color gradient for AQI values
AqiGradient <- c("Good.Days" = "#FDEB73",
                 "Moderate.Days" = "#F6C15B",
                 "Unhealthy.for.Sensitive.Groups.Days" = "#ED9445",
                 "Unhealthy.Days" = "#E66731",
                 "Very.Unhealthy.Days" = "#B84A29",
                 "Hazardous.Days" = "#6A3A2D")
# Gradient via https://blog.graphiq.com/finding-the-right-color-palettes-for-data-visualizations-fcd4e707a283

PollutantColumnNames <- c("Days.CO", "Days.NO2", "Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10")
PollutantFactorNames <- c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10", "Other")
pollutantPalette <- c(
  "CO" = "#E69F00",
  "NO2" = "#56B4E9",
  "Ozone" = "#F0E442",
  "SO2" = "#0072B2",
  "PM2.5" = "#D55E00",
  "PM10" = "#CC79A7",
  "Other" = "#999999")

# selectedYear <- 1980
# selectedState <- "Illinois"
# selectedCounty <- "Cook"

### pie chart showing the percentage of days, and a bar chart, and table
### showing the number of days where the AQI was good / moderate / unhealthy
### for sensitive / unhealthy / very unhealthy / hazardous

aqi_pie <- function(selectedYear, selectedState, selectedCounty) {
  # Select only year, state, and county - should be exactly 1 row
  aabc_yr <- subset(aabc, Year == selectedYear & State == selectedState & County == selectedCounty)

  # Short-circuit if invalid input passed to function
  if (nrow(aabc_yr) < 1) {
    return("NONE")
  }

  # Get number of days covered by dataset. If not 365 or 366 (leap yr), we are missing data
  daysWithAqi <- aabc_yr[["Days.with.AQI"]]

  # Select just these columns, convert to matrix, and transpose
  aqi_mx <- as.matrix(aabc_yr[AqiColumnNames])
  aqi_mx_t <- t(aqi_mx)

  # Convert back into dataframe and rename data column for reference
  aqi_mx_t_df <- data.frame(aqi_mx_t)
  colnames(aqi_mx_t_df) <- c("Days")

  # Convert row titles to a factor (like an enum) column so that ordering is preserved
  aqi_mx_t_df <- cbind(DayKind = factor(rownames(aqi_mx_t_df), levels=AqiColumnNames), aqi_mx_t_df)

  # Create percentage column
  aqi_mx_t_df$Pct <- with(aqi_mx_t_df, round((Days / daysWithAqi) * 100))

  # Create position column for labels
  aqi_mx_t_df$Endpt <- with(aqi_mx_t_df, cumsum(Days))
  aqi_mx_t_df$Midpt <- with(aqi_mx_t_df, Endpt - (Days / 2))
  aqi_mx_t_df$RealMidpt <- with(aqi_mx_t_df, (daysWithAqi - Midpt))

  ggplot(aqi_mx_t_df, aes(x="", y=Days, fill = DayKind, label=Pct)) +
    geom_bar(width = 1, stat="identity", position="stack") +
    coord_polar(theta="y", start=0) +
    theme_classic() +
    scale_fill_manual(values=AqiGradient) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(x=NULL, y=NULL, fill=NULL) +
    guides(fill=guide_legend(title="Day")) +
    geom_label(data=subset(aqi_mx_t_df, Pct > 0), aes(label=paste0(Pct, "%"), x=1.8, y=RealMidpt))
    #geom_label_repel(aes(label=paste0(Pct, "%"), x=1.5, y=RealMidpt)))
}

aqi_bar <- function(selectedYear, selectedState, selectedCounty) {
  # Select only year, state, and county - should be exactly 1 row
  aabc_yr <- subset(aabc, Year == selectedYear & State == selectedState & County == selectedCounty)

  if (nrow(aabc_yr) < 1) {
    return("NONE")
  }

  # Select just these columns, convert to matrix, and transpose
  aqi_mx <- as.matrix(aabc_yr[AqiColumnNames])
  aqi_mx_t <- t(aqi_mx)

  # Convert back into dataframe and rename data column for reference
  aqi_mx_t_df <- data.frame(aqi_mx_t)
  colnames(aqi_mx_t_df) <- c("Days")

  # Convert row titles to a factor (like an enum) column so that ordering is preserved
  aqi_mx_t_df <- cbind(DayKind = factor(rownames(aqi_mx_t_df), levels=AqiColumnNames), aqi_mx_t_df)

  ggplot(aqi_mx_t_df, aes(x=DayKind, y=Days, fill=DayKind)) +
    geom_col() + coord_flip() +
    scale_fill_manual(values=AqiGradient) +
    labs(y="Number of Days", x="Day Category") +
    guides(fill=guide_legend(title="Category")) +
    geom_text(aes(label=Days), nudge_y = 10)
}

aqi_table <- function(selectedYear, selectedState, selectedCounty) {
  # Select only year, state, and county - should be exactly 1 row
  aabc_yr <- subset(aabc, Year == selectedYear & State == selectedState & County == selectedCounty)

  if (nrow(aabc_yr) < 1) {
    return(data.frame(c("Insufficient data for this year"), c("...")))
  }

  # Select just these columns, convert to matrix, and transpose
  aqi_mx <- as.matrix(aabc_yr[AqiColumnNames])
  aqi_mx_t <- t(aqi_mx)

  # Convert back into dataframe and rename data column for reference
  aqi_mx_t_df <- data.frame(aqi_mx_t)
  colnames(aqi_mx_t_df) <- c("Days")

  # Clean up names
  aqi_mx_t_df$Category <- rownames(aqi_mx_t_df) %>%
    map(function(x) {gsub(".", " ", x, fixed=TRUE)}) %>%
    map(function(x) {gsub(" Days", "", x, fixed=TRUE)})
  return(aqi_mx_t_df[c("Category", "Days")])
}

### pie chart for each individual pollutant (CO, NO2, Ozone, SO2, PM2.5, PM10)
### showing the percentage of days in the year with that pollutant as the main
### pollutant

pollutant_pie <- function(selectedYear, selectedState, selectedCounty, pollutant) {
  #pollutant <- "Ozone"
  pollutantCol <- paste0("Days.", pollutant)

  # Select only year, state, and county - should be exactly 1 row
  aabc_yr <- subset(aabc, Year == selectedYear & State == selectedState & County == selectedCounty)
  if (nrow(aabc_yr) < 1) {
    return("NONE")
  }

  # Get number of days covered by dataset. If not 365 or 366 (leap yr), we are missing data
  daysWithAqi <- aabc_yr[["Days.with.AQI"]]

  daysWithPollutant <- aabc_yr[[pollutantCol]]

  df_types <- c(
   factor(pollutant, levels=PollutantFactorNames),
   factor("Other", levels=PollutantFactorNames)
  )
  df_days <- c(daysWithPollutant, daysWithAqi - daysWithPollutant)
  df_labels <- c(pollutant, "Other")
  df_label_positions <- c(round(daysWithAqi * .75), round(daysWithAqi * .25))

  pollutant_df <- data.frame(df_types, df_days, df_labels, df_label_positions, stringsAsFactors=FALSE)

  # Calculate percentages
  pollutant_df$pct <- with(pollutant_df, round((df_days / daysWithAqi) * 100))

  # Mucking around with factors so the colors come out right in ggplot
  pollutant_df$df_types <- with(pollutant_df, factor(df_labels, levels=rev(PollutantFactorNames), ordered = TRUE))

  ggplot(pollutant_df, aes(x="", y=df_days, fill = df_types, label=pct)) +
    geom_bar(width = 1, stat="identity", position="stack") +
    coord_polar(theta="y", start=0) +
    theme_classic() +
    #scale_fill_manual(aes(values = colormap))
    scale_fill_manual(values = pollutantPalette) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    labs(x=NULL, y=NULL, fill=NULL) +
    geom_label(
      data=subset(pollutant_df, df_types != "Other"),
      aes(label=paste0(df_labels, "\n", pct, "%")),
      color="white",
      size=10)
    #geom_label_repel(aes(label=paste0(df_labels, "\n", pct, "%")), color="white",size=10)
    #geom_label(aes(label=paste0(df_labels, "\n", pct, "%\n", as.integer(df_types))), color="white",size=10)
}

### bar chart and table showing the number of CO, NO2, Ozone, SO2, PM2.5, PM10
### days as the main pollutant

pollutant_bar <- function(selectedYear, selectedState, selectedCounty) {
  aabc_yr <- subset(aabc, Year == selectedYear & State == selectedState & County == selectedCounty)
  if (nrow(aabc_yr) < 1) {
    return("NONE")
  }
  # Select just these columns, convert to matrix, transpose,
  # Convert back into dataframe and rename data column for reference
  all_pollutants_df <- data.frame(t(as.matrix(aabc_yr[PollutantColumnNames])), stringsAsFactors=FALSE)
  colnames(all_pollutants_df) <- c("Days")
  all_pollutants_df$PollutantRowNames <- rownames(all_pollutants_df)
  all_pollutants_df$Pollutants <- with(all_pollutants_df, sub('.....', '', PollutantRowNames))

  ggplot(all_pollutants_df, aes(x=Pollutants, y=Days, fill=Pollutants, label=Days)) +
    geom_col() +
    scale_fill_manual(values = pollutantPalette) +
    labs(y="Number of Days", x="Pollutant") +
    geom_text(aes(label=Days), nudge_y = 10)
}

### show a line graph using the annual data from 1980-2018 showing lines for
### the median, 90th percentile, and max AQI over those years (i.e. the graph
### should have 3 lines)

aqi_line <- function(selectedState, selectedCounty) {
  aabc_region <- subset(aabc, State == selectedState & County == selectedCounty)[, AqiLineColumns]
  if (nrow(aabc_region) < 1) {
    return("NONE")
  }

  # We can't autoscale only one end; would like to go from 0 to max, but can't do that
  # For comparison, usually want to keep scale from 0 to 400, but if a value in the set is greater
  # than 400, use that as scale max
  aqiOverallMax = max(aabc_region$Max.AQI)
  ymax <- 400
  if (aqiOverallMax > 400) {
    ymax <- aqiOverallMax
  }

  ggplot(aabc_region) +
    geom_line(aes(x=Year, y=Max.AQI, color='red')) +
    geom_line(aes(x=Year, y=X90th.Percentile.AQI, color='orange')) +
    geom_line(aes(x=Year, y=Median.AQI, color='green')) +
    labs(x="Year", y="AQI") + theme_linedraw() +
    scale_x_continuous(limits=c(1980, 2018)) +
    scale_y_continuous(limits=c(0, ymax)) +
    scale_color_manual(name="AQI", values=c('red' = 'red', 'orange' = 'orange', 'green' = 'green'), labels=c('red' = "Maximum", 'orange' = "90th Percentile", 'green' = "Median"))
}

pollutant_line <- function(selectedState, selectedCounty) {
  pcColNames <- append(PollutantColumnNames, c("Year", "Days.with.AQI"))
  aabc_region <- subset(aabc, State == selectedState & County == selectedCounty)[, pcColNames]
  if (nrow(aabc_region) < 1) {
    return("NONE")
  }

  # Calculate percentage for each pollutant
  aabc_region$PctCO <- with(aabc_region, (Days.CO / Days.with.AQI) * 100)
  aabc_region$PctNO2 <- with(aabc_region, (Days.NO2 / Days.with.AQI) * 100)
  aabc_region$PctOzone <- with(aabc_region, (Days.Ozone / Days.with.AQI) * 100)
  aabc_region$PctSO2 <- with(aabc_region, (Days.SO2 / Days.with.AQI) * 100)
  aabc_region$PctPM2.5 <- with(aabc_region, (Days.PM2.5 / Days.with.AQI) * 100)
  aabc_region$PctPM10 <- with(aabc_region, (Days.PM10 / Days.with.AQI) * 100)

  ggplot(aabc_region) +
    geom_line(aes(x=Year, y=PctCO, color="CO")) +
    geom_line(aes(x=Year, y=PctNO2, color="NO2")) +
    geom_line(aes(x=Year, y=PctOzone, color="Ozone")) +
    geom_line(aes(x=Year, y=PctSO2, color="SO2")) +
    geom_line(aes(x=Year, y=PctPM2.5, color="PM2.5")) +
    geom_line(aes(x=Year, y=PctPM10, color="PM10")) +
    labs(x="Year", y="Percentage of Days") + theme_linedraw() +
    scale_color_manual(name="Pollutant", values=pollutantPalette)
}

pollutant_table <- function(selectedState, selectedCounty) {
  pcColNames <- append(PollutantColumnNames, c("Year", "Days.with.AQI"))
  aabc_region <- subset(aabc, State == selectedState & County == selectedCounty)[, pcColNames]
  if (nrow(aabc_region) < 1) {
    return("NONE")
  }

  aabc_region$Percent.CO <- with(aabc_region, round((Days.CO / Days.with.AQI) * 100), digits=1)
  aabc_region$Percent.NO2 <- with(aabc_region, round((Days.NO2 / Days.with.AQI) * 100), digits=1)
  aabc_region$Percent.Ozone <- with(aabc_region, round((Days.Ozone / Days.with.AQI) * 100), digits=1)
  aabc_region$Percent.SO2 <- with(aabc_region, round((Days.SO2 / Days.with.AQI) * 100), digits=1)
  aabc_region$Percent.PM2.5 <- with(aabc_region, round((Days.PM2.5 / Days.with.AQI) * 100), digits=1)
  aabc_region$Percent.PM10 <- with(aabc_region, round((Days.PM10 / Days.with.AQI) * 100), digits=1)

  return(aabc_region[c("Year", "Percent.CO", "Percent.NO2", "Percent.Ozone", "Percent.SO2", "Percent.PM2.5", "Percent.PM10")])
}

### show location of the chosen county on a pannable and zoomable world map
### with an appropriate background (that is reasonably centered and scaled on
### the US).

mapCounty <- function(selectedState, selectedCounty) {
  loc <- subset(locations, StateCounty == paste0(selectedState, "#", selectedCounty))
  if (nrow(loc) < 1) {
    # If no location found, just center map on center of US and don't add a marker
    return(leaflet() %>%
             addTiles() %>%
             setView(lat=31.51073, lng=-96.4247, zoom=5))
  }
  t <- loc$Latitude[1]
  n <- loc$Longitude[1]

  map <- leaflet() %>%
    addTiles() %>%
    addMarkers(lng=n, lat=t, popup=paste0(selectedCounty, " county, ", selectedState)) %>%
    setView(lat=t, lng=n, zoom=5)
}

