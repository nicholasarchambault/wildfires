###### Fire, Station, and City Data Read-in -  Script 1
###### Nicholas Archambault
###### Final Project, S&DS 425, 7 Dec. 2020

# This script completes the initial formulation and reading in of primary 
# CSV files used in this project. This code is separated out from other initial
# steps because the parsing of the SQL file and the querying of the CIMIS 
# database are lengthy processes. Script outputs three key CSV files; the only
# major file not handled by this script is the weather data.


# Import packages, including that which allows me to query CIMIS
library(RSQLite)
library(DBI)
library(rvest)
library(cimir)
set_key()      # Set API key for CIMIS


# Connect to SQLite server to read in table of 1.88 million wildfire records
con <- dbConnect(SQLite(), 
                 "/Users/nicholasarchambault/Desktop/FPA_FOD_20170508.sqlite")
# Convert desired table to data frame
as.data.frame(dbListTables(con))
fires <- dbReadTable(con, 'Fires')
# Output CSV
write.csv(fires, "fires.csv")



# Query CIMIS repository for information about California weather stations
stations <- cimis_station()
stations <- as.data.frame(stations)
# Write CSV
write.csv(stations, "stations.csv")



# Scrape Wikipedia for list of towns and cities in California. From this list,
# I will find longitude and latitude coordinates of each community, allowing me
# to understand each fire's proximity to the nearest community
url <- "https://en.wikipedia.org/wiki/List_of_cities_and_towns_in_California"
page <- read_html(url)
cities <- html_nodes(page, "tbody th a")
cities <- cities[6:length(cities)]
cities <- gsub("</a>", "", cities)
cities <- gsub("^.*>", "", cities)

ca <- matrix(data = NA, nrow = length(cities), ncol = 2)
ca[, 1] <- cities
ca[, 2] <- "California"

# Output CSV of cities, to be geocoding through internet website
write.csv(ca, "cities.csv")






