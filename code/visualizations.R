###### Visualizations - Script 4
###### Nicholas Archambault
###### Final Project, S&DS 425, 7 Dec. 2020

# This script performs a number of visualizations to manipulate and examine the
# combined fire, station, and weather data. It reads in multiple datasets and 
# outputs nine plots.


# Read in packages and data
library(usmap)
library(ggplot2)
library(treemap)
library(corrplot)
library(RColorBrewer)
library(Hmisc)
library(rgdal)

x <- read.csv("total_all.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Create log of size variable
x$log_size <- log(x$size)


#### PRELIMINARY DATA GROUPING

# Identify numerical columns
num_cols <- c(9, 15, 17:43, 45, 47:67)

# Group data by county
by_county <- aggregate(x[, num_cols], by = list(x$county), FUN = mean)
colnames(by_county)[1] <- "county"

# Read in dataset of population by county
pop <- read.csv("county_pop.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Perform minor cleaning
colnames(pop) <- c("county", "pop", "growth")
pop$county <- gsub(" County$", "", pop$county)
pop <- pop[, 1:2]

# Merge population data into county data
by_county <- merge(by_county, pop, by = c("county"))

# Aggregate to find cumulative acres burned by county
acres <- aggregate(x$size, by = list(x$county), FUN = sum)
colnames(acres) <- c("county", "acres_burnt")

# Merge acreage data with county data
by_county <- merge(by_county, acres, by = c("county"))

# Read in dataset of U.S. county FIPS codes (for use in map plots)
fips <- read.csv("fips.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Limit to only California counties
fips <- fips[fips$State == "California", ]

# Add leading zeros
fips$StateCode <- as.character(paste0("0", fips$StateCode))

# Create loop to add appropriate numbers of zeros to each county code in order
# to construct, in combination with state code, proper five-digit format
fips$fips <- NA
for (i in 1:nrow(fips)){
  if (nchar(fips[i, "CountyCode"]) == 1) 
  {fips[i, "fips"] <- as.character(paste0(fips[i, "StateCode"], "00", 
                                          fips[i, "CountyCode"]))}
  else if (nchar(fips[i, "CountyCode"]) == 2) 
  {fips[i, "fips"] <- as.character(paste0(fips[i,"StateCode"], "0", 
                                          fips[i, "CountyCode"]))}
  else if (nchar(fips[i, "CountyCode"]) > 2)
  {fips[i, "fips"] <- as.character(paste0(fips[i, "StateCode"], 
                                          fips[i, "CountyCode"]))}
}
fips <- fips[, c(2, 5)]
colnames(fips) <- tolower(colnames(fips))

# Merge fips data with county data
by_county <- merge(by_county, fips, by = "county")

# Define new variable: acres burned per county per 1,000 residents
by_county$acres_per_1000 <- 
  as.numeric(1000 * (by_county$acres_burnt / by_county$pop))


# Aggregate data by year from 1992 to 2015
by_year <- aggregate(x[, num_cols], by = list(x$year), FUN = mean)
colnames(by_year)[1] <- "year"

# Aggregate data by month across all years
by_month <- aggregate(x[, num_cols], by = list(x$month), FUN = mean)
top_fires <- x[order(x$size, decreasing = TRUE)[1:300], ]


#### CREATE PLOTS

# TREEMAP

# Create grouped factors from wildfire causes
values <- as.data.frame(table(x$cause))
values[, 3] <- c("Incendiary", "Incendiary", "Other", "Incendiary", "Equipment",
                 "Incendiary", "Lightning", "Other", "Other", "Equipment",
                 "Equipment", "Incendiary", "Other")
treemap(values, index = c("V3", "Var1"), vSize = "Freq", type = "index",
        fontsize.labels = c(15, 11),
        fontcolor.labels = c("black", "black"),
        bg.labels = c("transparent"),
        align.labels = list(c("left", "top"), c("center", "bottom")),
        border.col = c("black"),
        border.lwds = c(2),
        palette = c(rgb(204/255, 0, 0), 
                    rgb(1, 128/255, 0), 
                    rgb(1, 213/255, 0), 
                    rgb(160/255, 160/255, 160/255)),
        title = "Wildfire Causes"
)
# Note what miscellaneous means; note that no controlled burns specified



# CORRPLOT

# Identify columns to find correlations
cols <- c("duration", "eto", "prcp", "rel_hum_avg", "sol_rad_avg", 
          "wind_run", "eto_7", "prcp_7", "rel_hum_avg_7", "sol_rad_avg_7", 
          "wind_run_7", "eto_30", "prcp_30", "rel_hum_avg_30", "sol_rad_avg_30", 
          "wind_run_30","log_size")

# Create correlation matrix and plot
corrs <- rcorr(as.matrix(x[, cols]))

corrplot(corrs$r, method = "color",
         type = "upper", 
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE, 
         number.cex = 0.5,
         tl.cex = 0.6,
         p.mat = corrs$P,
         sig.level = 0.1
)



# MAPS

# Map of per-1,000 resident acres burned per county 
p1 <- plot_usmap("counties", 
                 include = "CA", 
                 data = by_county, 
                 values = "acres_per_1000", 
                 color = "black") + 
  labs(title = "Acres Burned Per 1000 County Residents") + 
  scale_fill_continuous(low = "white", 
                        high = "darkred", 
                        name = "Acres Per 1000", 
                        label = scales::comma) + 
  theme(legend.position = "right")

# Map of population per county
p2 <- plot_usmap("counties", 
                 include = "CA", 
                 data = by_county, 
                 values = "pop", 
                 color = "black") + 
  labs(title = "Population By County") + 
  scale_fill_continuous(low = "white", 
                        high = "darkblue", 
                        name = "Population", 
                        label = scales::comma) + 
  theme(legend.position = "right")

# Align these two maps on the same grid
gridExtra::grid.arrange(p1, p2, nrow = 1)


# Map of remoteness

# Convert remoteness from meters to kilometers
by_county$remoteness <- by_county$remoteness / 1000
p3 <- plot_usmap("counties", 
                 include = "CA", 
                 data = by_county, 
                 values = "remoteness", 
                 color = "brown") + 
  labs(title = "Average Distance of Fire Incidents From Nearest City/Town") + 
  scale_fill_continuous(low = "white", 
                        high = "darkgreen", 
                        name = "Remoteness (km)", 
                        label = scales::comma) + 
  theme(legend.position = "right")


# Transform data appropriately for longitude-latitude plotting
lat_lon <- usmap_transform(top_fires[, c("longitude", "latitude", "size")])
lat_lon <- cbind(lat_lon, top_fires$size)

# Map of California with 300 largest fires highlighted
p4 <- plot_usmap(include = "CA") +
  geom_point(data = lat_lon, 
             aes(x = longitude.1, y = latitude.1, size = size),
             color = "orangered2", 
             alpha = 0.6) +
  labs(title = "Most Devastating 300 California Wildfires",
       size = "Acres Burned") +
  theme(legend.position = "right")



# Group remoteness into leveled factor based on distance ranges
rem <- x[, c("year", "station_number", "size", "duration", "remoteness")]
rem$remoteness <- rem$remoteness / 1000

rem$score <- NA
rem[rem$remoteness < 25, "score"] <- "Very Near\n(0 - 25 km away)"
rem[(rem$remoteness >= 25) & (rem$remoteness < 50), "score"] <- 
  "Near\n(26 - 50 km away)"
rem[(rem$remoteness >= 50) & (rem$remoteness < 75), "score"] <- 
  "Comfortable\n(51 - 75 km away)"
rem[(rem$remoteness >= 75) & (rem$remoteness < 100), "score"] <- 
  "Remote\n(76 - 100 km away)"
rem[rem$remoteness >= 100, "score"] <- 
  "Very Remote\n(over 100 km away)"

rem$score <- factor(rem$score)

# Aggregate size and duration of fires based on remoteness level
rem_agg <- aggregate(rem[, c("size", "duration")], list(rem$score), mean)

# Define order of levels for use in plot
rem_agg$ordered <- 
  factor(rem_agg$Group.1, c("Very Remote\n(over 100 km away)",
                            "Remote\n(76 - 100 km away)",
                            "Comfortable\n(51 - 75 km away)",
                            "Near\n(26 - 50 km away)",
                            "Very Near\n(0 - 25 km away)"))

# Plot of average fire size by remoteness level
p5 <- ggplot(rem_agg, aes(x = ordered, y = size)) +
  geom_bar(stat = "identity", width = 0.5, fill = "darkgreen") + 
  labs(title = "Average Size by Proximity", 
       x = "Proximity of Fire to Nearest City/Town", 
       y = "Acres Burned") +
  coord_flip() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 16, face = "bold")) +
  geom_hline(data = x, yintercept = as.numeric(mean(x$size)), 
             col = "firebrick", size = 1.25) +
  annotate("text", 
           x = 5, 
           y = 190, 
           label = "Average CA Wildfire Size", 
           color = "firebrick")

# Plot of average fire duration by remoteness level
p6 <- ggplot(rem_agg, aes(x = ordered, y = duration)) +
  geom_bar(stat = "identity", width = 0.5, fill = "sienna4") + 
  labs(title = "Average Duration by Proximity", 
       x = "Proximity of Fire to Nearest City/Town", 
       y = "Days") +
  coord_flip() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 16, face = "bold")) +
  geom_hline(data = x, yintercept = as.numeric(mean(x$duration)), 
             col = "blue", size = 1.25) +
  annotate("text", 
           x = 5, 
           y = 1.2, 
           label = "Average CA Wildfire Duration", 
           color = "blue")



# BUBBLE PLOT

# Define new factor with levels of wind run
top_fires$w <- NA
top_fires[top_fires$wind_run < 130, "w"] <- "Low Wind"
top_fires[(top_fires$wind_run >= 130) & (top_fires$wind_run < 160), "w"] <- 
  "Moderate Wind"
top_fires[(top_fires$wind_run >= 160) & (top_fires$wind_run < 200), "w"] <- 
  "High Wind"
top_fires[top_fires$wind_run >= 200, "w"] <- "Extreme Wind"
top_fires$w <- factor(top_fires$w, levels = c("Extreme Wind",
                                              "High Wind",
                                              "Moderate Wind",
                                              "Low Wind"))

# Bubble plot of maximum air temp by minimum relative humidity, where size of 
# bubble represents size of fire and color represents strength of wind
ggplot(top_fires, 
       aes(x = air_tmp_max, y = rel_hum_min, size = size, color = w)) + 
  labs(colour = "Wind Strength") +
  annotate("rect", 
           xmin = 25, 
           xmax = 50, 
           ymin = 0, 
           ymax = 50, 
           alpha = .2, 
           fill = "firebrick") +
  annotate("text", 
           x = 40, 
           y = 53, 
           label = "Elevated Fire Risk", 
           color = "firebrick") +
  geom_point(alpha = 0.7) +
  xlim(0, 50) + 
  ylim(0, 100) +
  scale_size(range = c(1, 14), 
             name = "Size of Fire (Acres)") + 
  scale_color_manual(values = c(rgb(0/255, 25/255, 51/255), 
                                rgb(0/255, 76/255, 153/255),
                                rgb(51/255, 153/255, 255/255),
                                rgb(204/255, 229/255, 255/255))) +
  xlab("Daily Avg. Air Temperature (C)") + 
  ylab("Daily Avg. Relative Humidity (%)") +
  ggtitle(label = "Fire Risk Factors", 
          subtitle = "Top 300 Largest CA Wildfires, 1992-2015")


# LINE PLOT OF WEATHER TRENDS

# Identify columns to examine
cols <- c("year", "month", "size", "duration", "air_tmp_avg", "prcp", 
          "rel_hum_avg", "sol_rad_net")

# Randomly sample 1000 datapoints from each year 
samp <- list()
for (i in 1992:2015) {
  a <- x[(x$year == i) & 
           (x$month == "June" | x$month == "July" | x$month == "August"), cols]
  a <- a[, -2]
  s <- aggregate(a, list(a$year), mean)
  samp <- append(samp, list(s))
}
samp <- do.call(rbind, samp)
samp <- samp[, -1]

# Standardize data to non-dimensional scale between 0 and 1
standardize <- function(x) {x <- ((x - min(x)) / (max(x) - min(x)))}
samp[, 2:ncol(samp)] <- apply(samp[, 2:ncol(samp)], 2, standardize)

# Fit linear model for each column, predicted by year, to observe trends from 
# 1992 to 2015
fit <- lapply(samp[, 2:ncol(samp)], function(x) lm(x ~ year, data = samp))

# Store linear model slopes and intercepts
ints <- list()
slopes <- list()
for (i in 1:length(fit)) {
  ints <- append(ints, list(fit[[i]]$coefficients[1]))
  slopes <- append(slopes, list(fit[[i]]$coefficients[2]))
}
ints <- unlist(as.numeric(ints))
slopes <- unlist(as.numeric(slopes))

# Create plot with best fit lines for overall 24-year trends of each 
# meteorological variable
plot(c(1992, 2015), c(0, 1), type = "n", xlab = "", ylab = "")
abline(b=slopes[1], a=ints[1], col="firebrick", lwd = 3, lty = 2)
abline(b=slopes[2], a=ints[2], col="steelblue", lwd = 3, lty = 2)
abline(b=slopes[3], a=ints[3], col="forestgreen", lwd = 3, lty = 2)
abline(b=slopes[4], a=ints[4], col="black", lwd = 3, lty = 2)
abline(b=slopes[5], a=ints[5], col="darkviolet", lwd = 3, lty = 2)
abline(b=slopes[6], a=ints[6], col="darkgoldenrod2", lwd = 3)
legend("topright", legend = c("Net Solar Radiation", "Relative Humidity", 
                              "Avg. Air Temp.", "Precip.", "Size", "Duration"),
       col = c("darkgoldenrod2", "darkviolet", "forestgreen", "black", 
               "firebrick", "steelblue"),
       pch = 19)
title(main = "Meteorological Trends\nSummer Months, 1992-2015", 
      sub = "(Dashed lines indicate trends contributing to or indicative of worsening fire conditions)", 
      cex.sub = 1)



