################################################################################
# FLOW WAVE LENGTH
# Author: Hana Thurman
# November 22, 2024

# Description: 
# Estimating the spatial length of flow waves from SWOT data.
################################################################################


################################################################################
# Load libraries
################################################################################

if (!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")}; require(ggplot2)
if (!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")}; require(tidyverse)


################################################################################
# Input variables
################################################################################
# SWOT data: River single-pass vector node data for the river stretch of interest
# Were filtered, smoothed, and interpolated.
# A temporal median water surface elevation by node was also computed.
# Result is this "SWOT_data_reconstructed.csv" file.
SWOT_data_reconstructed <- read.csv("SWOT_data_reconstructed.csv")

# River single-pass vector node datasets can be downloaded from EarthData search
# https://search.earthdata.nasa.gov/
# Combined multitemporal data from each pass by merging shapefiles in ArcPro.
# Manually selected the nodes from the river of interest in ArcPro.
# Note: Most river stretches are covered by more than one pass.
# To cover all data, this may require merging multiple pass shapefiles togther.
# Exported to csv; result is this 'SWOT_profiles.shp'
SWOT_profiles <- read.csv("SWOT_profiles.csv")

# Flow wave date: Expressed as a "time_id" field in the dataset above.
# Should choose the date the flow wave was observed by SWOT.
# In this example, we selected 3/12/2024 for the Ocmulgee River flow wave.
timeId <- "212098"


################################################################################
# Step 1: Find 90th percentile water surface elevation by node
################################################################################
# Find max dist and calculate downstream distance field in km.
max_dist_SWOT_data_reconstructed <- max(SWOT_data_reconstructed$dist_out)
SWOT_data_reconstructed$Diff_km <- 
  ((max_dist_SWOT_data_reconstructed - SWOT_data_reconstructed$dist_out))/1000

# Filter to only flow wave date.
SWOT_data_reconstructed <- SWOT_data_reconstructed |>
  filter(time_id == timeId)

# Clean and prep data.
# Remove invalid water surface elevations and bad quality nodes.
SWOT_profiles <- SWOT_profiles[-which(SWOT_profiles$wse <= -1000000),]
SWOT_profiles <- filter(SWOT_profiles, node_q < 3)

# Select only nodes on flow wave stretch.
SWOT_profiles <- SWOT_profiles[SWOT_profiles$node_id %in% 
                                 SWOT_data_reconstructed$node_id,]

# Calculate 90th percentile wse at each node.
peak_wse_conditions <-
  SWOT_profiles |>
  group_by(node_id) |>
  summarize(wse_90 = quantile(wse, na.rm = T, probs = 0.9))

peak_wse_conditions$node_id <- as.numeric(peak_wse_conditions$node_id)

################################################################################
# Step 2: Find which nodes are part of flow wave
################################################################################
# Join to original dataframe.
SWOT_data_reconstructed <-
  left_join(SWOT_data_reconstructed, peak_wse_conditions, by = "node_id")

# Determine which nodes are >=90th percentile (part of flow wave).
# Binary tag, 1 = flow wave.
SWOT_data_reconstructed <-
  SWOT_data_reconstructed |>
  mutate(flow_wave = ifelse(bayes_wse >= wse_90, 1, 0))

SWOT_data_reconstructed$flow_wave <- as.character(SWOT_data_reconstructed$flow_wave)

# Plot to identify consecutive nodes tagged "flow wave."
# We considered the flow wave to start once 10 or more nodes exceeded the threshold
# And end when 10+ nodes dropped below the threshold.
SWOT_data_reconstructed |>
  ggplot() +
  geom_point(aes(x = Diff_km, y = bayes_wse, color = flow_wave)) +
  labs(x = "Distance downstream", y = "SWOT WSE (m)")
