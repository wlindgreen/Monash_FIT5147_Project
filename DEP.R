# Data Exploration Project

# Import libary ####
library("rjson")
library("geojsonR")
library("rstudioapi")
library(broom)
library(scales)
library(ggplot2)
library(leaflet)
library(dplyr)
library(rgdal)
library(jsonlite)
library(geosphere)
library(geojsonio)
library(lubridate)
library(htmlwidgets)
library(htmltools)



# Import data ####

# Import travel time files
setwd(dirname(getActiveDocumentContext()$path))
data <- read.csv("Data_cleaned/2020_1_DatesByHourBucketsAggregate.csv", sep = ",", header = TRUE)
data_hour_all <- read.csv("Data_cleaned/2020_1_All_HourlyAggregate.csv", sep = ",", header = TRUE)
data_hour_weekend <- read.csv("Data_cleaned/2020_1_OnlyWeekend_HourlyAggregate.csv", sep = ",", header = TRUE)
data_hour_weekday <- read.csv("Data_cleaned/2020_1_OnlyWeekdays_HourlyAggregate.csv", sep = ",", header = TRUE)
data_month <- read.csv("Data_cleaned/MonthlyAggregate.csv", sep = ",", header = TRUE)

# Import weather data
weather <- read.csv("Data_cleaned/15_20_Weather.csv", sep = ",", header = TRUE)
weather$date <- as.Date(weather$date)

# Formatting and second to minutes
data$date <- as.Date(data$date)
data$mean_travel_time <- data$mean_travel_time/60
data$sd_travel_time <- data$sd_travel_time/60

data_hour_all$mean_travel_time <- data_hour_all$mean_travel_time/60
data_hour_all$sd_travel_time <- data_hour_all$sd_travel_time/60
data_hour_weekend$mean_travel_time <- data_hour_weekend$mean_travel_time/60
data_hour_weekend$sd_travel_time <- data_hour_weekend$sd_travel_time/60
data_hour_weekday$mean_travel_time <- data_hour_weekday$mean_travel_time/60
data_hour_weekday$sd_travel_time <- data_hour_weekday$sd_travel_time/60

data_month$mean_travel_time <- data_month$mean_travel_time/60
data_month$sd_travel_time <- data_month$sd_travel_time/60


# TEST IMPORT ZONES AS JSON ----
spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")
spdf

# Load objects
zones <- jsonlite::fromJSON("Data_cleaned/melbourne_tz.json")
# Check your region list
head(zones$features$properties)
# Polygon coordinates for each region
str(zones$features$geometry$coordinates)

# load objects
zones_geo <- jsonlite::fromJSON("Data_cleaned/melbourne_tz.geojson")
# Check your region list
head(zones_geo$features$properties)
# Polygon coordinates for each region
str(zones_geo$features$geometry$coordinates)

mel_polygons <- zones_geo$features$geometry$coordinates
mel_temp_poly <- mel_polygons[[1]]
mel_poly_len <- length(mel_temp_poly)/2
mel_poly_df <- data.frame(lng = mel_temp_poly[1,1:mel_poly_len,1], lat = mel_temp_poly[1,1:mel_poly_len,2])
mel_my_poly_matrix <- data.matrix(mel_poly_df)
mel_temp_centroid <- centroid(mel_my_poly_matrix)

leaflet(mel_temp_centroid) %>% 
  addTiles() %>% 
  addMarkers() %>% 
  addPolygons(lng = mel_poly_df$lng, lat= mel_poly_df$lat)


# IMPORT ZONES AS SHAPEFILE ----
mel_shp <- readOGR("Data_cleaned/melbourne_tz/melbourne_tz.shp",
                  layer = "melbourne_tz", GDAL1_integer64_policy = TRUE)

# Only zones
leaflet(mel_shp) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5)




# 3. Data checking ####
# 


# Check for zones without Uber trips (MISSING VALUES) ----

# Calculate TT for all fromzones
from_zones <- data %>% group_by(sourceid) %>%
  summarise(avg_TT = mean(mean_travel_time),
            .groups = 'drop')

# Derive travel time from zones to array
TT_fromzone <- from_zones$avg_TT[match(mel_shp$MOVEMENT_I, from_zones$sourceid)]

# Color mapping and labels for the map
# Source: https://rstudio.github.io/leaflet/choropleths.html
cpal <- colorNumeric("Blues", TT_fromzone)
labels <- sprintf(
  "<strong>%s</strong>",
  mel_shp$MOVEMENT_ID
) %>% lapply(htmltools::HTML)

# Title for map
tag.map.title <- tags$style(HTML(".leaflet-control.map-title {transform: translate(-50%,20%); position: fixed !important;
    left: 50%;text-align: center;padding-left: 10px; padding-right: 10px; background: rgba(255,255,255,0.75);
    font-weight: bold;font-size: 16px;
  }"))
title <- tags$div(tag.map.title, HTML("Average travel time from zones in Melbourne"))  


# Create Choropleth Map
leaflet(data = mel_shp) %>% # create a blank canvas
  #addProviderTiles(providers$Stamen.Toner)  %>%
  # Add polygons and features
  addPolygons( # draw polygons on top of the base map (tile)
    stroke = FALSE, 
    smoothFactor = 0.2, 
    fillOpacity = 0.9,
    weight = 2,
    color = ~cpal(TT_fromzone), # Use the ID of each zone to find the correct color
    
    # Add interactions when hovering over zones
    # Highlighting polygons
    highlightOptions = highlightOptions(
      weight = 10,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    # Labels
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px",direction = "auto")
    # Legend
  ) %>%
  addLegend(pal = cpal, values = ~TT_fromzone, opacity = 0.7, title = "Avg. Travel Time <br>[min.]",
            position = "bottomleft") %>%
  # Title
  addControl(title, position = "topleft", className = "map-title")



# Check for dates without Uber trips (STRUCTURAL ERROR) ----

# Calculate avg. TT for dates
gb_dates <- data1 %>% group_by(date) %>%
  summarise(avg_TT = mean(mean_travel_time),
            .groups = "drop")

dates_vline <- as.Date(c("2020-03-16"))                 # Define positions of vline
dates_vline <- which(gb_dates$date %in% dates_vline)

# Plot showing travel time each day
ggplot(data = gb_dates, aes(x = date, y = avg_TT)) +
  geom_point(fill = 'orange', na.rm = TRUE, size = 4, color = "black", pch=21) +
  geom_vline(xintercept = as.numeric(gb_dates$date[dates_vline]), color = "red", lwd = 1.5, linetype="dashed") +
  xlab("Date") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time between all zones in Melbourne (1/1/2020 - 31/3/2020)") + 
  scale_x_date(labels = date_format("%d-%m"), breaks = gb_dates$date[seq(1, length(gb_dates$date), by = 3)]) +
  theme_linedraw() + theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.4))

# REMOVAL OF DATES AFTER 15/03
data1 <- data[data[["date"]] <= "2020-03-15", ]

# Calculate avg. TT for dates new
gb_dates_1 <- data1 %>% group_by(date) %>%
  summarise(avg_TT = mean(mean_travel_time),
            .groups = "drop")

ggplot(data = gb_dates_1, aes(x = avg_TT)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.3) +
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept = mean(avg_TT)),
             color="blue", linetype="dashed", size=1)


# Calculate avg. TT for year and month
gb_YM <- data_month %>% group_by(year, month) %>%
  summarise(avg_TT = mean(mean_travel_time),
            .groups = "drop")
gb_YM$YM <- make_date(year = gb_YM$year, month = gb_YM$month) # New column with merge of Year and month

# Plot showing travel time each day
ggplot(data = gb_YM, aes(x = YM, y = avg_TT)) +
  geom_point(fill = 'orange', na.rm = TRUE, size = 4, color = "black", pch = 21) +
  xlab("Month") + ylab("Average Travel Time (min.)") +
  ggtitle("Monthly average travel time between all zones in Melbourne (2015-2020)") + 
  scale_x_date(labels = date_format("%m-%y"), breaks = gb_YM$YM[seq(1, length(gb_YM$YM), by = 1)]) + 
  theme_linedraw() + theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4))


# Check for outliers  (OUTLIER ERROR) ----

ggplot(data = data_BT, aes(x = dow, y = mean_travel_time)) + 
  geom_boxplot()

ggplot(data = data1, aes(x = "", y = mean_travel_time)) + 
  geom_boxplot(fill = "orange") + 
  theme_minimal()

ggplot(data = weather, aes(x = "", y = log(Rainfall_mm + 1))) +
  geom_violin( fill='#A4A4A4', color="darkred") +
  xlab("") + ylab("Log(Rainfall + 1)") +
  ggtitle("Violinplot of the log transformed amount of rainfall (2015-2020)") +
  theme_linedraw() + theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4))


# 4. Data Exploration ####

# Q1. Hourly patterns ----
gb_hod <- data_hour_all %>% group_by(hod) %>%
  summarise(avg_TT = mean(mean_travel_time),.groups = "drop")
gb_hod_weekend <- data_hour_weekend %>% group_by(hod) %>% 
  summarise(avg_TT = mean(mean_travel_time),.groups = "drop")
gb_hod_weekend$cat <- "Weekend"
gb_hod_weekday <- data_hour_weekday %>% group_by(hod) %>% 
  summarise(avg_TT = mean(mean_travel_time),.groups = "drop")
gb_hod_weekday$cat <- "Weekday"
gb_hod_rb <- rbind(gb_hod_weekend, gb_hod_weekday)


# Plot
ggplot(data = gb_hod_rb, mapping = aes(x = hod, y = avg_TT, group = cat)) + 
  geom_line(size = 1, aes(color = cat)) +
  scale_color_manual(values = c('#32CD32', '#663399')) +
  geom_point() +
  xlab("Hour of day") + ylab("Average Travel Time (min.)") +
  ggtitle("Hourly average travel time between all zones in Melbourne") + 
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  theme_linedraw() + theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.9),
    legend.text = element_text(size = 12),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4),
    legend.title=element_blank()
    )


# Q1. Weekly patterns ----
# Group by weekday and hour buckets
gb_hourB_wk <- data1 %>% group_by(dow, day_name, hourB) %>%
  summarise(avg_TT = mean(mean_travel_time),.groups = "drop")
# New name for weekday with only three letters
gb_hourB_wk$day_name_abb <- substr(gb_hourB_wk$day_name, 1, 3)
# Merge weekday name and hour buckets columns
gb_hourB_wk$dow_HB <- paste(gb_hourB_wk$day_name_abb, gb_hourB_wk$hourB)

gb_hourB_wk$dow_HB <-factor(gb_hourB_wk$dow_HB,levels = c("Mon 0 - 7", "Mon 7 - 10", "Mon 10 - 16", "Mon 16 - 19", "Mon 19 - 0", 
        "Tue 0 - 7", "Tue 7 - 10", "Tue 10 - 16", "Tue 16 - 19", "Tue 19 - 0", 
        "Wed 0 - 7", "Wed 7 - 10", "Wed 10 - 16", "Wed 16 - 19", "Wed 19 - 0",  
        "Thu 0 - 7", "Thu 7 - 10", "Thu 10 - 16", "Thu 16 - 19", "Thu 19 - 0",
        "Fri 0 - 7", "Fri 7 - 10", "Fri 10 - 16", "Fri 16 - 19", "Fri 19 - 0", 
        "Sat 0 - 7", "Sat 7 - 10", "Sat 10 - 16", "Sat 16 - 19", "Sat 19 - 0",
        "Sun 0 - 7","Sun 7 - 10", "Sun 10 - 16", "Sun 16 - 19", "Sun 19 - 0"))

gb_hourB_wk <-gb_hourB_wk[
  with(gb_hourB_wk, order(dow_HB)),]

ggplot(data = gb_hourB_wk, mapping = aes(x = dow_HB, y = avg_TT)) + 
  geom_bar(fill = 'orange', stat = 'identity', colour = "black") +
  xlab("Hour Bucket of the Week") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time between all zones in Melbourne divided by hour buckets and weekday") +
  scale_y_continuous(limits = c(0,14), expand = c(0, 0)) +
  theme_linedraw() + theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.9),
    legend.text = element_text(size = 12),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4),
    legend.title = element_blank()
  )


# Try with color specific bars
co <- list("Standard", "Highest", "Standard", "Standard", "Lowest",
           "Standard", "Highest", "Standard", "Standard", "Lowest",
           "Standard", "Standard", "Standard", "Highest", "Lowest",
           "Standard", "Highest", "Standard", "Standard", "Lowest",
           "Lowest", "Standard", "Standard", "Highest", "Standard",
           "Standard", "Lowest", "Highest", "Standard", "Standard",
           "Highest", "Lowest", "Standard", "Standard", "Standard")
gb_hourB_wk$co <- co
gb_hourB_wk <-as.data.frame(lapply(gb_hourB_wk, unlist))

ggplot(data = gb_hourB_wk, mapping = aes(x = dow_HB, y = avg_TT, fill = as.factor(co))) + 
  geom_bar(stat = 'identity', colour = "black") +
  scale_fill_manual(name = "Hour bucket of the day", values = c("Standard" = "grey80", "Highest" = "darkred",
                                                          "Lowest" = "darkgreen")) + 
  xlab("Hour Bucket of the Week") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time between all zones in Melbourne divided by hour buckets") +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  theme_linedraw() + theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.91, 0.92),
    legend.text = element_text(size = 12),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4)
    #, legend.title = element_blank()
  )


# Q1. Yearly patterns by month ----

#Month data only for Burnley Tunnel
data_month_BT<- data_month[data_month[['dstid']] == "962", ]

# Add seasons
data_month_BT$season <- ifelse(data_month_BT$month_name %in% c("Dec", "Jan", "Feb") , "Summer",
                                      ifelse(data_month_BT$month_name %in% c("Mar", "Apr", "May"), "Autumn",
                                             ifelse(data_month_BT$month_name  %in% c("Jun", "Jul", "Aug"), "Winter", "Spring")))


# Group by month
gb_month_season <- data_month_BT %>% group_by(month) %>%
  summarise(avg_TT = mean(mean_travel_time), avg_sd = mean(sd_travel_time),.groups = "drop")
gb_month_season$month_name <- month.abb[gb_month_season$month]

gb_month_season$month_name <-factor(gb_month_season$month_name, levels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Calculate pct. from January
gb_month_season$jan_pct <- (gb_month_season$avg_TT - 24.15)/24.15*100
gb_month_season[gb_month_season$month == 1, "jan_pct"] <- 0


# Line with error bars
ggplot(data = gb_month_season, mapping = aes(x = month_name, y = avg_TT ,group = 1)) +
  geom_line(color = '#663399', size = 2) +
  geom_errorbar(
      aes(ymin = avg_TT-avg_sd, ymax = avg_TT+avg_sd),
      position = position_dodge(0.1), width = 0.2, color = '#E69F00') +
  geom_point(size = 4, color = '#663399') +
  xlab("Month") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time to Burnley Tunnel divided by month (avg. standard deviation included)") +
  theme_linedraw() + theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    panel.grid.minor.y = element_blank(), panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.9), legend.text = element_text(size = 12),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4),
    legend.title = element_blank() 
  )

# Bar chart
ggplot(data = gb_month_season, mapping = aes(x = month_name, y = avg_TT)) + 
  geom_bar(fill = 'orange', stat = 'identity', colour = "black") +
  #geom_point() +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat = "count", vjust = -.5) +
  xlab("Month") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time to Burnley Tunnel divided by month (avg. standard deviation included") +
  theme_linedraw() + theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    panel.grid.minor.y = element_blank(), panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.9), legend.text = element_text(size = 12),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4),
    legend.title = element_blank()
  )


# Q1. Seasonal travel patterns, 2015 – 2020 ----

# Airport ONLY ====

#Month data only for Tullmarine Airport 
data_month_AirP<- data_month[data_month[['dstid']] == "362", ]

# Add seasons
data_month_AirP$season <- ifelse(data_month_AirP$month_name %in% c("Dec", "Jan", "Feb") , "Summer",
                               ifelse(data_month_AirP$month_name %in% c("Mar", "Apr", "May"), "Autumn",
                                      ifelse(data_month_AirP$month_name  %in% c("Jun", "Jul", "Aug"), "Winter", "Spring")))

gb_month_AirP <- data_month_AirP %>% group_by(YM, month, month_name, year, season) %>%
  summarise(avg_TT = mean(mean_travel_time), avg_sd = mean(sd_travel_time),.groups = "drop")

# Plot showing travel time month 2015-2020 (Burnley - Airport)
ggplot(data = gb_month_AirP, aes(x = YM, y = avg_TT)) +
  geom_line() +
  geom_point( na.rm = TRUE, size = 4, pch = 21, aes( fill = factor(season))) +
  xlab("Year and month") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time to Tullamarine Airport over the years") + 
  scale_fill_manual(values = c("Summer" = "#FFD700", "Winter"="#42687C",
                               "Spring"="orange","Autumn" = "#603C14")) +
  scale_x_date(labels = date_format("%b-%y"), breaks = gb_YM$YM[seq(1, length(gb_YM$YM), by = 1)]) + 
  theme_linedraw() + theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.grid.minor.y = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    legend.position = c(0.93, 0.85),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4)) +
  guides(fill=guide_legend(title="Season"))


# Burnley tunnel and Airport Burnley tunnel and St Kilda ====

#Month and year between Burnley tunnel and St Kilda 
data_month_BT_SKroad <- data_month_BT[data_month_BT[['sourceid']] == "643", ]
data_month_BT_SKroad$YM <- make_date(month = data_month_BT_SKroad$month, year = data_month_BT_SKroad$year) # New column with merge of Year and month

# Plot showing travel time month 2015-2020 (Burnley - St Kilda)
ggplot(data = data_month_BT_SKroad, aes(x = YM, y = mean_travel_time)) +
  geom_line() +
  geom_point( na.rm = TRUE, size = 4, pch = 21, aes( fill = factor(season))) +
  xlab("Year and month") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time between Burnley Tunnel and St Kilda over the years") + 
  scale_fill_manual(values = c("Summer" = "#FFD700", "Winter"="#42687C",
                                "Spring"="orange","Autumn" = "#603C14")) +
  scale_x_date(labels = date_format("%b-%y"), breaks = gb_YM$YM[seq(1, length(gb_YM$YM), by = 1)]) + 
  theme_linedraw() + theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.grid.minor.y = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    legend.position = c(0.93, 0.85),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4)) +
  guides(fill=guide_legend(title="Season"))

# Burnley tunnel and Airport Burnley tunnel and Airport ====
#Month and year between Burnley tunnel and Airport
data_month_BT_AirP <- data_month_BT[data_month_BT[['sourceid']] == "362", ]
data_month_BT_AirP$YM <- make_date(month = data_month_BT_AirP$month, year = data_month_BT_AirP$year) # New column with merge of Year and month

# Plot showing travel time month 2015-2020 (Burnley - Airport)
ggplot(data = data_month_BT_AirP, aes(x = YM, y = mean_travel_time)) +
  geom_line() +
  geom_point( na.rm = TRUE, size = 4, pch = 21, aes(fill = factor(season))) +
  xlab("Year and month") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time between Burnley Tunnel and Tullamarine Airport over the years") + 
  scale_fill_manual(values = c("Summer" = "#FFD700", "Winter"="#42687C",
                               "Spring"="orange","Autumn" = "#603C14")) +
  scale_x_date(labels = date_format("%b-%y"), breaks = gb_YM$YM[seq(1, length(gb_YM$YM), by = 1)]) + 
  theme_linedraw() + theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.grid.minor.y = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    legend.position = c(0.93, 0.85),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4)) +
  guides(fill=guide_legend(title="Season"))



# Q2. TT From tunnel  ----

# Shapefile Melbourne zones
mel <- geojson_read("Data_cleaned/melbourne_tz.json",  what = "sp")
mel@data$MOVEMENT_ID
mel_tb <- tidy(mel)
mel_tb$id <- as.integer(mel_tb$id)

# New data set with only Burnley Tunnel
data_BT <- data1[data1[['dstid']] == "962" , ]

gb_data_BT <- data_BT %>% group_by(sourceid) %>%
  summarise(avg_TT_BT = mean(mean_travel_time),.groups = 'drop')
colnames(gb_data_BT)[which(names(gb_data_BT) == "sourceid")] <- "id"

# Merge zone data with TT BT and remove NA
mel_tb_BT <- mel_tb %>%left_join(. , gb_data_BT, by = c("id"="id"))
mel_tb_BT <- mel_tb_BT[!is.na(mel_tb_BT$avg_TT_BT), ] # Remove NA Values

# Color
pal <- hcl.colors(6, "Blues 3", rev = TRUE, alpha = 0.7)

ggplot() + 
  geom_polygon(data = mel_tb_BT, aes(fill = avg_TT_BT, x = long, y = lat, group = id)) +
  theme_void() +
  coord_map()

# With monthly BT 2016 and 2019
data_month_BT_2016 <- data_month_BT[data_month_BT[['year']] == "2016", ]
gb_data_month_BT_2016 <- data_month_BT_2016 %>% group_by(sourceid) %>%
  summarise(avg_TT_BT_2016 = mean(mean_travel_time),.groups = 'drop')
colnames(gb_data_month_BT_2016)[which(names(gb_data_month_BT_2016) == "sourceid")] <- "id"

data_month_BT_2019 <- data_month_BT[data_month_BT[['year']] == "2019", ]
gb_data_month_BT_2019 <- data_month_BT_2019 %>% group_by(sourceid) %>%
  summarise(avg_TT_BT_2019 = mean(mean_travel_time),.groups = 'drop')
colnames(gb_data_month_BT_2019)[which(names(gb_data_month_BT_2019) == "sourceid")] <- "id"
gb_data_month_BT_16_19 <- gb_data_month_BT_2016 %>% left_join(. , gb_data_month_BT_2019, by = c("id"="id"))
gb_data_month_BT_16_19$diff_16_19 <- (gb_data_month_BT_16_19$avg_TT_BT_2019 - gb_data_month_BT_16_19$avg_TT_BT_2016) / gb_data_month_BT_16_19$avg_TT_BT_2016 * 100


# Merge zone data with TT BT and remove NA
mel_tb_BT <- mel_tb_BT %>% left_join(. , gb_data_month_BT_16_19, by = c("id"="id"))

# Diff
mel_tb_BT$diff_16_20 <- (mel_tb_BT$avg_TT_BT_2019 - mel_tb_BT$avg_TT_BT_2016) / mel_tb_BT$avg_TT_BT_2016

#mel_tb_BT <- mel_tb_BT[!is.na(mel_tb_BT$avg_TT_BT_2019) ,] # Remove NA Values
#mel_tb_BT <- mel_tb_BT[!is.na(mel_tb_BT$avg_TT_BT_2016) ,] # Remove NA Values

ggplot() + 
  geom_polygon(data = mel_tb_BT, aes(fill = diff_16_20, x = long, y = lat, group = id)) +
  theme_void() +
  coord_map()

# Leaflet map

#Colours and values
tt_diff <- gb_data_month_BT_16_19$diff_16_19[match(mel_shp$MOVEMENT_I, gb_data_month_BT_16_19$id)]
cpal <- colorNumeric("RdYlGn_r", tt_diff)

# Title for map
tag.map.title <- tags$style(HTML(".leaflet-control.map-title {transform: translate(-50%,20%); position: fixed !important;
    left: 50%;text-align: center;padding-left: 10px; padding-right: 10px; background: rgba(255,255,255,0.75);
    font-weight: bold;font-size: 16px;
  }"))
title <- tags$div(tag.map.title, HTML("Pct. difference in travel time between 2016 and 2019 from Burnley Tunnel"))  

leaflet(data = mel_shp) %>% # create a blank canvas
  #addProviderTiles(providers$Stamen.Toner)  %>%
  # Add polygons and features
  addPolygons( # draw polygons on top of the base map (tile)
    stroke = FALSE, 
    smoothFactor = 0.2, 
    fillOpacity = 0.9,
    weight = 2,
    color = ~cpal(tt_diff), # Use the ID of each zone to find the correct color
    
    # Add interactions when hovering over zones
    # Highlighting polygons
    highlightOptions = highlightOptions(
      weight = 10,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
      
    ),
    # Labels
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    # Legend
  ) %>%
  addLegend(pal = cpal, values = ~tt_diff, opacity = 0.7, title = "Avg. Travel Time <br>[pct.]",
            position = "bottomright") %>%
  # Title
  addControl(title, position = "topleft", className = "map-title")


# Ekstra ====
# Recover row name
#MOV_ID <- data.frame(mel@data$MOVEMENT_ID)
#names(MOV_ID) <- c("MOVEMENT_ID")
# Create and append "id"
#MOV_ID$id <- seq(0,nrow(MOV_ID)-1)
#mel_tb <- join(mel_tb, MOV_ID, by = "id")

# New data set with only Albert park
#data_AP <- data1[data1[['dstid']] == "1000" , ]


#gb_data_AP <- data_AP %>% group_by(sourceid) %>%
#  summarise(avg_TT_AP = mean(mean_travel_time),.groups = 'drop')
#colnames(gb_data_AP)[which(names(gb_data_AP) == "sourceid")] <- "id"


# Q2. TT Difference from tunnel  ----

# Q3. Influence from weather ----

gb_dates <- data1 %>% group_by(date) %>%
    summarise(avg_TT = mean(mean_travel_time),
              .groups = "drop")
  
gb_dates <- merge(gb_dates, weather, by = 'date')
gb_dates$log_rainfall <- log(1+gb_dates$Rainfall_mm)

gb_dates$group <- ifelse(gb_dates$Rainfall_mm == 0, "0",
                         ifelse(gb_dates$Rainfall_mm < 1, "0-1",
                                ifelse(gb_dates$Rainfall_mm < 3, "1-3",
                                       ifelse(gb_dates$Rainfall_mm < 6, "3-6", "6+"))))

gb_group_rain <- gb_dates %>% group_by(group) %>%
  summarise(avg_TT = mean(avg_TT),.groups = 'drop')
gb_group_rain <- na.omit(gb_group_rain)

ggplot(data = gb_group_rain, mapping = aes(x = group, y = avg_TT)) + 
  geom_bar(fill = 'orange', stat = 'identity', colour = "black") +
  xlab("Rainfall (mm)") + ylab("Average Travel Time (min.)") +
  ggtitle("Average travel time by rainfall grouping") +
  scale_y_continuous(limits = c(0,14), expand = c(0, 0)) +
  theme_linedraw() + theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.9),
    legend.text = element_text(size = 12),
    legend.background = element_rect(color = "black", linetype = "solid", size = 0.4),
    legend.title = element_blank()
  )

# Play ####

gb_data_month <- data_month %>% group_by(year, month) %>%
  summarise(avg_TT = mean(mean_travel_time),
            .groups = 'drop')


# Centroids of zones
library(rgeos)
trueCentroids = gCentroid(mel_shp, byid=TRUE)
plot(mel_shp)
points(coordinates(mel_shp), pch = 1)
points(trueCentroids, pch=2)


# Export data to DVP ####

# HOUR OF DAY
write.csv(gb_hod_rb,"/Users/williamlindgreen/MONASH/FIT5147/Project/Data_DVP/gb_hod.csv", row.names = FALSE)

# HOUR BUCKETS OF THE WEEK
write.csv(gb_hourB_wk,"/Users/williamlindgreen/MONASH/FIT5147/Project/Data_DVP/gb_hourB_wk.csv", row.names = FALSE)

# TRAVEL TIME ZONES and DEVELOPMENT
# St Kilda Beach = 1017, Airport = 362 , Burnley Tunnel = 962, Marvel Stadium = 960 , 927 = Carlton Gardens
data_month_5zone <- data_month[data_month[['sourceid']] %in% c("362", "1017", "962", "960", "927"), ]
data_month_5zone$month_name <- month.abb[data_month_5zone$month]
data_month_5zone$YM <- make_date(month = data_month_5zone$month, year = data_month_5zone$year) # New column with merge of Year and month
data_month_5zone$season <- ifelse(data_month_5zone$month_name %in% c("Dec", "Jan", "Feb") , "Summer",
                                  ifelse(data_month_5zone$month_name %in% c("Mar", "Apr", "May"), "Autumn",
                                         ifelse(data_month_5zone$month_name  %in% c("Jun", "Jul", "Aug"), "Winter", "Spring")))

# TRAVEL TIME DEVELOPMENT 5 ZONES
data_month_5zone_dev <- data_month_5zone[data_month_5zone[['dstid']] %in% c("362", "1017", "962", "960", "927"), ]
write.csv(data_month_5zone_dev,"/Users/williamlindgreen/MONASH/FIT5147/Project/Data_DVP/data_month_5zone_dev.csv", row.names = FALSE)

# TRAVEL TIME ZONES
data_month_5zone_map <- data_month_5zone %>% group_by(sourceid, dstid, year) %>%
  summarise(avg_travel_time = mean(mean_travel_time),.groups = 'drop')
write.csv(data_month_5zone_map,"/Users/williamlindgreen/MONASH/FIT5147/Project/Data_DVP/data_month_5zone_map.csv", row.names = FALSE)

