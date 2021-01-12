###### Bulk Cleaning and Dataframe Tweaking - Script 3
###### Nicholas Archambault
###### Final Project, S&DS 425, 7 Dec. 2020

# This script reads in the 'total' CSV, the dataframe containing all information
# on fires, their associated stations, and weather measurements made at the time
# of their burns. It pares this large dataframe down to its most essential
# elements and creates new lagged time series variables for specific parameters


# Read in packages and data
library(RANN)
library(zoo)
library(data.table)
library(sp)
library(geosphere)
total <- read.csv("total.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Eliminate unnecessary columns
total <- total[, -c(1, 4, 18, 24:25)]

# Eliminate all rows with at least one NA value -- these will hinder later
# modeling attempts
total <- na.omit(total)

#### DATA CLEANING

# Convert column names to desired length, case and format using 'gsub'
colnames(total)[16:42] <- gsub("^Value.Day", "", colnames(total)[16:42])
colnames(total)[16:42] <- gsub("\\..*", "", colnames(total)[16:42])
colnames(total)[16:42] <- 
  gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", colnames(total)[16:42])
colnames(total)[16:42] <- tolower(colnames(total)[16:42])
colnames(total)[16:42] <- gsub("precip", "prcp", colnames(total)[16:42])
colnames(total)[16:42] <- gsub("vap_pres", "vp", colnames(total)[16:42])

# Eliminate duplicate rows
total <- unique(total)

# Eliminate extraneous values. Soil temperature restricted to be 0 or positive
# in order to eliminate likely mistakes
total <- total[total$soil_tmp_avg >= 0, ]

# Precipitation restricted to realistic values
total <- total[total$prcp != 6999, ]

# Relative humidity is a percentage and cannot be negative
total <- total[total$rel_hum_min >= 0, ]

# ETo should not be negative but sometimes is due to rounding errors or other
# arithmetical flukes. Re-adjust all values so that they are 0 or greater
total$eto <- total$eto - min(total$eto)



#### INCORPORATING CITIES

# Read in dataframe of geocoding list of cities in California
cities <- read.csv("geocode.csv")

# Fix columns
cities <- cities[, c(2:5, 14)]
colnames(cities) <- c("city", "state", "latitude", "longitude", "county")

# Eliminate mistakes listing lat and long values as 0
cities <- cities[(cities$latitude != 0) & (cities$longitude != 0), ]

# Clean county names
cities$county <- gsub(" County$", "", cities$county)

# Trim tail end of dataframe
endpoint <- which(cities$city == "")[1] - 1
cities <- cities[1:endpoint, ]

# Define new columns in 'total' dataframe
total$closest_city <- NA
total$remoteness <- NA
total$county <- NA

# Create pairs of city coordinates, as well as pairs of fire coordinates
c_coords <- cities[, c(4:3)]
f_coords <- total[, c("longitude", "latitude")]

# Use 'geosphere' packages to calculate the distances between all fires and
# all California cities. Identify the closest city to each fire, and the
# distance between the two points
sp.f_coords <- f_coords
sp.c_coords <- c_coords
d <- distm(sp.f_coords, sp.c_coords, distCosine)
min.d.ind <- apply(d, 1, function(x) order(x, decreasing = FALSE)[1])
min.d <- apply(d, 1, min)


# Fill newly-defined columns appropriately with city name, county name, and
# distance between city and fire
total$closest_city <- cities[min.d.ind, "city"]
total$county <- cities[min.d.ind, "county"]
total$remoteness <- min.d

# Eliminate few mistakes in data
total <- total[total$closest_city != "", ]


# Order dataframe by station number and start date in preparation to create
# lagged columns
total <- total[order(total$station_number, total$start_date), ]



#### CREATE LAGGED VARIABLES

# Define variables that will be lagged
lagged_cols <- total[, c(1, 2, 16, 19:22, 25, 28, 30, 37:38)]

# Split dataframe by station
s <- split(lagged_cols, factor(lagged_cols$station_number))

# Create loop that replaces every mini station-associated dataframe 
# with the average of that dataframe's variables over the preceding 7 days
lagged <- list()
for (i in 1:length(s)) {
  b <- frollmean(as.data.frame(s[i])[, 3:12], n = 7)
  m <- matrix(unlist(b), nrow = nrow(as.data.frame(s[i])[, 3:12]), 
              ncol = 10, byrow = FALSE)
  s[[i]][, 3:12] <- m
  lagged <- append(lagged, list(s[[i]]))
}

lagged7 <- do.call(rbind, lagged)

# Repeat for the values of the variables over the preceding 30 days. We now have
# week- and month-lagged columns of most numerical weather variables.
lagged <- list()
for (i in 1:length(s)) {
  b <- frollmean(as.data.frame(s[i])[, 3:12], n = 30)
  m <- matrix(unlist(b), nrow = nrow(as.data.frame(s[i])[, 3:12]), 
              ncol = 10, byrow = FALSE)
  s[[i]][, 3:12] <- m
  lagged <- append(lagged, list(s[[i]]))
}

lagged30 <- do.call(rbind, lagged)

# Bind lagged columns with 'total' dataframe
colnames(lagged7) <- paste0(colnames(lagged7), "_7")
colnames(lagged30) <- paste0(colnames(lagged30), "_30")

lagged7 <- lagged7[, -c(1:2)]
lagged30 <- lagged30[, -c(1:2)]

total <- cbind(total, lagged7, lagged30)

# Write CSV
total <- na.omit(total)
write.csv(total, "total_all.csv")





#### DEFINE UNITS OF NUMERICAL COLUMNS

# size [acres]
# air_tmp_avg [C]
# air_tmp_max [C]
# air_tmp_min [C]
# dew_pnt [C]
# eto [mm]
# prcp [mm]
# rel_hum_avg [%]
# rel_hum_max [%]
# rel_hum_min [%]
# soil_tmp_avg [C]
# soil_tmp_max [C]
# soil_tmp_min [C]
# sol_rad [W -m^2]
# sol_rad_net [W -m^2]
# vp_avg [kPa]
# vp_max [kPa]
# vp_min [kPa]
# wind_ene [m -s]
# wind_ese [m -s]
# wind_nne [m -s]
# wind_nnw [m -s]
# wind_ssw [m -s]
# wind_sse [m -s]
# wind_wnw [m -s]
# wind_wsw [m -s]
# wind_spd_avg [m -s]
# wind_run [m -s]


