################################################################################
# FLOW WAVE FINDER
# Author: Hana Thurman
# November 22, 2024

# Description: 
# General workflow for finding flow waves from SWOT data starting 
# with US Geological Survey gauge height variations.
################################################################################


################################################################################
# Load libraries
################################################################################

if (!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")}; require(ggplot2)
if (!"dataRetrieval" %in% rownames(installed.packages())){
  install.packages("dataRetrieval")}; require(dataRetrieval)
if (!"sf" %in% rownames(installed.packages())){
  install.packages("sf")}; require(sf)
if (!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")}; require(tidyverse)


################################################################################
# Input variables
################################################################################
# Dates: Adjust to fit your needs. A bit of relevant information...
# Example shows pulling all available data from SWOT science orbit at the time.
# Science orbit began 7/26/2023 and is ongoing as of Nov. 2024.
# Fast sampling/calval orbit lasted from 3/29/2023 to 7/11/2023.
startDate <- "2023-07-26"
endDate <- "2024-11-22"

# States: Where to search for flow waves (US state abbreviations).
# Example uses all states in the country. Note that it takes a while to run.
# Suggest specifying your own custom list of states if appropriate.
states <- state.abb

# SWORD: SWOT River Database
# Downloaded v16 from https://www.swordexplorer.com/
# Merged reach files from North America, then clipped to US boundary in ArcPro.
# Result is this 'SWORD_v16_US_shp'
SWORD_v16_US_shp <- "SWORD_v16_US.shp"

# Variability threshold: Only look at gauges with highest variation in stage.
# Example uses "5" (i.e., standard deviation of stage is >= 5 ft)
threshold <- 5

# SWOT data: Only applicable if you want to try plotting SWOT data after
# Finding potential flow waves from the gauges.
# River single-pass vector node datasets can be downloaded from EarthData search
# https://search.earthdata.nasa.gov/
# Combined multitemporal data from each pass by merging shapefiles in ArcPro.
# Manually selected the nodes from the river of interest in ArcPro.
# Exported to csv; result is this 'SWOT_profiles.shp'
SWOT_profiles <- read.csv("SWOT_profiles.csv")


################################################################################
# Step 1: Identify US-based gauges measuring stage
################################################################################
# Narrow down to gauges that:
# (1) Measure gauge height;
# (2) Provide daily values;
# (3) Are located within the states of interest;
# (4) Collected data during our time period of interest.

for (i in 1:length(states)){
  if (i <= 1){
    gaugeNum <- whatNWISdata(stateCd = states[i],
                             parameterCd = "00065",
                             service = "dv") |>
      filter(begin_date <= startDate & end_date > startDate)
    
    sitesGaugeHeight <- gaugeNum
  }
  else {
    gaugeNum <- whatNWISdata(stateCd = states[i],
                             parameterCd = "00065",
                             service = "dv") |>
      filter(begin_date <= startDate & end_date > startDate)
    
    sitesGaugeHeight <- rbind(sitesGaugeHeight, gaugeNum)
  }
}


################################################################################
# Step 2: Spatially select gauges located on SWOT-observable rivers
################################################################################
# Read SWORD shapefile.
SWORD_v16_US <- st_read(SWORD_v16_US_shp)

# Buffer SWORD reaches by 1km (Need to get in projected CRS first. 
# I chose simple Mercator).
SWORD_v16_US <- st_transform(SWORD_v16_US, crs = 3395)
SWORD_v16_US_buff <- st_buffer(SWORD_v16_US, 1000)

# Create a gauge shp and select gauges located within the buffers.
Gauges_all <- st_as_sf(sitesGaugeHeight, 
                       coords = c("dec_long_va", "dec_lat_va"), crs = 4326)
Gauges_all <- st_transform(Gauges_all, crs = 3395)
Gauges_SWOT_science <- Gauges_all[SWORD_v16_US_buff, ]

# Return list of gauges.
site_no_list <- as.list(Gauges_SWOT_science$site_no)


################################################################################
# Step 3: Calculate the standard deviation by month at each gauge
################################################################################
# Calculate variability for each gauge/month
# This could be modified to change the time scale if desired.
for (i in 1:length(site_no_list)){
  if(i <= 1){
    gaugeDat <- readNWISdv(siteNumber = site_no_list[i],
                           parameterCd = "00065", #Gauge height
                           startDate = startDate,
                           endDate = endDate) |>
      renameNWISColumns()
    
    if(nrow(gaugeDat) > 0 & "GH" %in% colnames(gaugeDat)){
      gaugeDat <- gaugeDat |>
        mutate(monthYear = format(as.Date(Date), "%m-%Y")) |>
        group_by(monthYear) |>
        summarize(GH_sd = sd(GH))
      
      gaugeDat$site_no <- site_no_list[i]
      gaugeHeightStdDev <- gaugeDat
    }
  }
  else{
    gaugeDat <- readNWISdv(siteNumber = site_no_list[i],
                           parameterCd = "00065", #Gauge height
                           startDate = startDate,
                           endDate = endDate) |>
      renameNWISColumns()
    
    if(nrow(gaugeDat) > 0 & "GH" %in% colnames(gaugeDat)){
      gaugeDat <- gaugeDat |>
        mutate(monthYear = format(as.Date(Date), "%m-%Y")) |>
        group_by(monthYear) |>
        summarize(GH_sd = sd(GH))
      
      
      gaugeDat$site_no <- site_no_list[i]
      gaugeHeightStdDev <- rbind(gaugeHeightStdDev, gaugeDat)
    }
    
  }
}

#Select gauges with highest standard deviation based on threshold.
gaugeHeightStdDevHigh <-
  gaugeHeightStdDev |>
  filter(GH_sd >= threshold)


################################################################################
# Output variables
################################################################################
# Gauges with a monthly standard deviation above selected threshold.
# High variability in gauge height suggests that a flow wave might have occurred.
# At this point, we manually inspected the USGS data to confirm flow waves,
# And identify which we might want to plot with SWOT data.
gaugeHeightStdDevHigh


################################################################################
# Example of how you might plot the SWOT water surface elevation profiles
################################################################################
# Clean and prep data.
# Remove invalid water surface elevations and bad quality nodes.
SWOT_profiles <- SWOT_profiles[-which(SWOT_profiles$wse <= -1000000),]
SWOT_profiles$ymd <- ymd_hms(SWOT_profiles$time_str)
SWOT_profiles$Date <- format(as.Date(SWOT_profiles$ymd), "%m-%d-%y")
SWOT_profiles$Date <- as.factor(SWOT_profiles$Date)
SWOT_profiles <- filter(SWOT_profiles, node_q < 3)

# Find max dist and calculate downstream distance field in km.
max_dist_SWOT_profiles <- max(SWOT_profiles$p_dist_out)
SWOT_profiles$Diff_km <- 
  ((max_dist_SWOT_profiles - SWOT_profiles$p_dist_out))/1000

# Plot.
SWOT_profiles |>
  ggplot() +
  geom_line(aes(Diff_km, wse, color = Date)) +
  labs(x = "Distance downstream (km)", y = "Water surface elevation (m)")
