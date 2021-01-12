###### Initial Cleaning and Weather Data - Script 2
###### Nicholas Archambault
###### Final Project, S&DS 425, 7 Dec. 2020

# This script performs initial cleaning of the fire and stations dataframes
# prior to using the information from the stations dataframe to query the CIMIS
# API for all pertinent weather data.


# Read in packages and fires dataframe
rm(list = ls())
library(cimir)
library(chron)
library(RSQLite)
library(DBI)
library(lubridate)
library(RANN)
library(beepr)
library(sp)
library(geosphere)

fires <- read.csv("fires.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Filter 'fires.csv' for only California fires; adjust column names; check
# for NA's
fires <- fires[fires$STATE == "CA", ]
colnames(fires) <- tolower(colnames(fires))

fires <- fires[, c("fire_name", "fire_year", "discovery_date", "discovery_doy",
                   "stat_cause_descr", "cont_date", "cont_doy", "fire_size", 
                   "fire_size_class", "latitude", "longitude", "owner_descr")]

colnames(fires) <- c("name", "year", "start_date", "start_doy", "cause", 
                     "end_date", "end_doy", "size", "class", "latitude", 
                     "longitude", "owner")

colSums(is.na(fires))


# Remove all fires without a confirmed end date
fires <- fires[!is.na(fires$end_date), ]

# Create function to convert fire start and end dates to proper format. Dates
# created in Julian format denoting absolute time since some day of origin 
# (in this case, the standard origin: 24 November 4713 BC). This function
# converts any date from Julian to familiar Gregorian format
julian_converter <- function(x) {
  z <- month.day.year(x, c(month = 11, day = 24, year = -4713))
  m <- matrix(data = NA, nrow = length(z$month), ncol = 4)
  m[, 1] <- z$month
  m[, 2] <- z$day
  m[, 3] <- z$year
  m[, 4] <- paste0(m[, 1], "-", m[, 2], "-", m[, 3])
  return(as.Date(m[, 4], format = "%m-%d-%Y"))
}

# Apply function to start and end dates
fires$start_date <- julian_converter(fires$start_date)
fires$end_date <- julian_converter(fires$end_date)

# Create month and duration variables
fires$month <- month.name[month(fires$start_date)]
fires$duration <- fires$end_doy - fires$start_doy

# Eliminate all fires which began in one year and ended in another. This will
# eliminate most mistaken entries in the data as well as reduce the tedium of
# processing fires that crossed into a new year.
fires <- 
  fires[(year(as.Date(fires$start_date)) == year(as.Date(fires$end_date))), ]

# Eliminate all fires with negative durations (these are mistakes in the data)
# as well as all fires that ended prior to 1 January 1992
fires <- fires[fires$duration >= 0, ]
fires <- fires[fires$end_date > "1992-01-01", ]



#### HANDLE STATION DATA FOR EACH FIRE

# Read in stations file
stations <- read.csv("stations.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Eliminate unnecessary columns; adjust column names
colnames(stations)

stations <- stations[, c("StationNbr", "Name", "County", "ConnectDate", 
                         "DisconnectDate", "IsActive", "HmsLatitude", 
                         "HmsLongitude", "ZipCodes")]

colnames(stations) <- c("number", "name", "county", "connect", "disconnect", 
                        "active", "lat", "long", "zipcodes")

# Clean longitude and latitude columns
stations$lat <- gsub("^.* / ", "", stations$lat)
stations$long <- gsub("^.* / ", "", stations$long)

# Convert date columns to date objects
connect <- strptime(as.character(stations$connect), "%m/%d/%Y") 
stations$connect <- format(connect, "%Y-%m-%d")
disconnect <- strptime(as.character(stations$disconnect), "%m/%d/%Y")
stations$disconnect <- format(disconnect, "%Y-%m-%d")

# Eliminate all stations that were disconnected prior to 1992, the start year
# of the fires dataframe, as well as those which only connected after 2015, the
# end year
stations <- stations[year(as.Date(stations$disconnect)) > 1992, ]
stations <- stations[year(as.Date(stations$connect)) < 2015, ]

# Define unique stations dataframe that eliminates duplicate entries
stations_u <- unique(stations[, 1:8])

# Define new columns in fires dataframe, to be filled momentarily
fires$station_name <- NA
fires$station_number <- NA

# Create loop that processes which station was closest to each fire at the time
# it happened. We cannot naively use simple nearest neighbors search; we must 
# take the connection and disconnection dates of each station into account in 
# order to ensure that the geographically closest station was actually active at
# the time of a particular blaze

for (row in 1:nrow(fires)) {
  f_coords <- fires[row, c("longitude", "latitude")]
  beg <- as.Date(fires[row, "start_date"])
  end <- as.Date(fires[row, "end_date"])
  s_coords <- 
    stations_u[((stations_u$connect < beg) & (stations_u$disconnect > end)), 
               c("long", "lat")]
  sp.f_coords <- f_coords
  sp.s_coords <- s_coords
  
  d <- distm(sp.f_coords, sp.s_coords, distCosine)
  min.d.ind <- apply(d, 1, function(x) order(x, decreasing = FALSE)[1])
  
  fires[row, "station_number"] <- stations_u$number[min.d.ind]
  fires[row, "station_name"] <- stations_u$name[min.d.ind]
  
}

# Output CSV containing information on the fires and the stations which
# monitored nearby weather conditions on the day of the blaze
write.csv(fires, "fires_stations.csv")



#### QUERY CIMIS API FOR WEATHER DATA

# Define desired meteorological parameters to be queried from CIMIS
parameters <- cimis_items()[, 2][1:29]

# Create loop that identifies the earliest and latest activation dates for each
# sensor, then queries the API in batches of 1,750 records (the maximum number
# allowed per query) in order to gather all weather information for all stations
# throughout their lifetime
ww <- list()
for (i in 1:nrow(fires)) {
  remove_key()
  set_key() 
  
  s <- max(c(as.Date("1992-01-01"), 
             as.Date(stations_u[i, "connect"])))
  e <- s + 1749
  end <- min(c(as.Date("2015-12-31"), 
               as.Date(stations_u[i, "disconnect"])))
  while ((e <= end) & (e > s)){
    x <- cimis_data(targets = stations_u[i, "number"], 
                    start.date = s,
                    end.date = e, 
                    items = parameters,
                    measure.unit = "M")
    
    x <- as.data.frame(x)
    x <- x[, c("Date", "Station", "Item", "Value", "Unit")]
    x$Item <- paste(x$Item, x$Unit, sep = " ")
    x <- x[, c("Date", "Station", "Item", "Value")]
    
    ww <- append(ww, list(reshape(x, idvar = "Date", v.names = "Value", 
                                  timevar = "Item", direction = "wide")))
    Sys.sleep(1)
    
    s <- e + 1
    if (end - e > 1749) {e <- e + 1749}
    else {e <- end}
  }
  Sys.sleep(1)
}

# Output CSV of all weather data
weather <- do.call(rbind, ww)
write.csv(weather, "weather.csv")


# Merge fires data with weather data by data and station. Each fire is assigned
# the weather data from the nearest station on the day it burned
total <- merge(fires, weather, by.x = c("start_date", "station_number"), 
               by.y = c("Date", "Station"))


# Output CSV of all combined data
write.csv(total, "total.csv")
