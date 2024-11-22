################################################################################
# SPATIAL HYDROGRAPH SEPARATION
# Author: Hana Thurman
# November 22, 2024

# Description: 
# Applying a digital filter from Chapman and Maxwell (1996) to separate
# baseflow from total flow for both SWOT and gauge data.
################################################################################


################################################################################
# Load libraries
################################################################################

if (!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")}; require(ggplot2)
if (!"dataRetrieval" %in% rownames(installed.packages())){
  install.packages("dataRetrieval")}; require(dataRetrieval)
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

# Flow wave date: Expressed as a "time_id" field in the dataset above.
# Should choose the date the flow wave was observed by SWOT.
# In this example, we selected 3/12/2024 for the Ocmulgee River flow wave.
timeId <- "212098"

# Site number: ID of USGS gauge closest to peak of SWOT spatial hydrograph.
# Examples are for Ocmulgee River flow wave.
site_no <- "02215000"

# Dates: Time duration of gauge data you want to use.
# Examples are for Ocmulgee River flow wave.
startDate <-  "2024-03-07"
endDate <- "2024-03-19"

# Time zone where the gauge is located.
# E.g., "America/New_York", "America/Chicago", "America/Denver",
# "America/Los_Angeles"
timeZone <- "America/New_York"

# SWOT baseflow recession coefficient: Manually enter this value after
# Completing step 1.
# Example value is provided.
k_coeff_SWOT <- 0.9876067


# Gauge baseflow recession coefficient: Manually enter this value after
# Completing step 3.
# Example value is provided.
k_coeff_gauge <- 0.9975


################################################################################
# Step 1: Estimate recession coefficient k for SWOT data
################################################################################
# Calculate relative water surface elevation using reconstructed data
# And temporal median values.
SWOT_data_reconstructed <-
  SWOT_data_reconstructed |>
  mutate(relative_wse = wse - wse_reference,
         relative_bayes_wse = bayes_wse - wse_reference)

# Find max dist and calculate downstream distance field in km.
max_dist_SWOT_data_reconstructed <- max(SWOT_data_reconstructed$dist_out)
SWOT_data_reconstructed$Diff_km <- 
  ((max_dist_SWOT_data_reconstructed - SWOT_data_reconstructed$dist_out))/1000

# Filter to only flow wave date.
SWOT_data_reconstructed <- SWOT_data_reconstructed |>
  filter(time_id == timeId)

# Semilogarithmic plot.
# Recession segment corresponds to upstream portion of plot.
# Three components (quickflow, interflow, and baseflow) plot approximately
# As three straight lines.
# Manually identify beginning and end of final (baseflow) recession segment.
# Plug these into equation below as Qt and Q0 to get α.
# Qt = Q0 e^(-αt)
# Find k using α.
# k = e^(-α)
SWOT_data_reconstructed |>
  ggplot() +
  geom_point(aes(x = Diff_km, y = relative_bayes_wse)) +
  scale_y_log10() +
  labs(x = "Distance downstream (km)", y = "WSE median absolute deviation (m)")


################################################################################
# Step 2: Apply digital filter to partition SWOT spatial hydrograph
################################################################################
# Filter through data and calculate base wse.
for(i in 1:length(SWOT_data_reconstructed$relative_bayes_wse)){
  if(i <= 1){
    base_wse <- SWOT_data_reconstructed$relative_bayes_wse[1]
  }
  else {
    base_wse <- (((k_coeff_SWOT/(2 - k_coeff_SWOT)) * SWOT_data_reconstructed[i-1, 11])) 
    + (((1 - k_coeff_SWOT)/(2 - k_coeff_SWOT)) * (SWOT_data_reconstructed$relative_bayes_wse[i]))
  }
  SWOT_data_reconstructed$base_wse[i] <- base_wse
  }


# Plot (part of manuscript Figure 3.)
SWOT_data_reconstructed |>
  ggplot() + 
  geom_line(aes(x = Diff_km, y = relative_bayes_wse, color = "Total WSE"), size = 0.4) +
  geom_line(aes(x = Diff_km, y = base_wse, color = "Base WSE"), size = 0.4) +
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8), 
        legend.position = "none") +
  scale_x_reverse() +
  labs(x = "Distance downstream (km)", y = "WSE median absolute deviation (m)")


################################################################################
# Step 3: Estimate recession coefficient k for gauge data
################################################################################
# Get discharge data using USGS dataRetrieval package.
gauge_dat <- readNWISuv(siteNumbers = site_no,
                        parameterCd = "00060",
                        startDate = startDate,
                        endDate = endDate,
                        tz = timeZone) |> 
  renameNWISColumns()

#Start by converting dateTime to minutes
# For easier percentage calculations at the end.
for(i in 1:length(gauge_dat$dateTime)){
  duration <- as.numeric(difftime(gauge_dat$dateTime[1], gauge_dat$dateTime[i], units = "mins")) 
  gauge_dat$mins[i] <- duration}

gauge_dat$mins <- as.numeric(gauge_dat$mins) * -1

# Semilogarithmic plot.
# Recession segment corresponds to upstream portion of plot.
# Three components (quickflow, interflow, and baseflow) plot approximately
# As three straight lines.
# Manually identify beginning and end of final (baseflow) recession segment.
# Plug these into equation below as Qt and Q0 to get α.
# Qt = Q0 e^(-αt)
# Find k using α.
# k = e^(-α)
#Recession coefficient plot
gauge_dat |>
  ggplot() +
  geom_point(aes(x = dateTime, y = Flow_Inst)) +
  scale_y_log10() +
  labs(x = "Time", y = "Discharge (cfs)")


################################################################################
# Step 4: Apply digital filter to partition gauge (temporal) hydrograph
################################################################################
# Filter through data and calculate baseflow.
for(i in 1:length(gauge_dat$Flow_Inst)){
  if(i <= 1){
    baseflow <- gauge_dat$Flow_Inst[1]
  }
  else {
    baseflow <- (((k_coeff_gauge/(2 - k_coeff_gauge)) * gauge_dat[i-1, 10])) + 
      (((1 - k_coeff_gauge)/(2 - k_coeff_gauge)) * (gauge_dat$Flow_Inst[i]))
  }
  gauge_dat$baseflow[i] <- baseflow
}


# Plot (part of manuscript Figure 3.)
gauge_dat |>
  mutate(Flow_Inst_cms = 0.02831683199881 * Flow_Inst,
         Baseflow_cms = 0.02831683199881 * baseflow) |> #Cfs to cms conversion
  ggplot() + 
  geom_line(aes(x = dateTime, y = Flow_Inst_cms, color = "Total flow"), size = 0.4) +
  geom_line(aes(x = dateTime, y = Baseflow_cms, color = "Baseflow"), size = 0.4) +
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8), 
        legend.text = element_text(size = 8), 
        legend.title = element_blank(), legend.position = "none") +
  scale_x_datetime(date_labels = "%b %d", breaks = "3 days") +
  labs(x = "Time", y = "Discharge (cms)")


################################################################################
# Step 5: Calculate percentage baseflow
################################################################################
# Calculating area under curve - SWOT.
# Total area
swotTotal <- integrate(approxfun(SWOT_data_reconstructed$Diff_km, 
                                 SWOT_data_reconstructed$relative_bayes_wse), 
                       lower = 0, upper = max(SWOT_data_reconstructed$Diff_km), subdivisions = 1000)
# Base wse
swotBase <- integrate(approxfun(SWOT_data_reconstructed$Diff_km, 
                                SWOT_data_reconstructed$base_wse), 
                      lower = 0, upper = max(SWOT_data_reconstructed$Diff_km), subdivisions = 1000)

swotPercentage <-
  swotBase$value / swotTotal$value

# Calculating area under curve - Gauge.
# Total area
gaugeTotal <- integrate(approxfun(gauge_dat$mins, gauge_dat$Flow_Inst), 
                        lower = 0, upper = max(gauge_dat$mins), subdivisions = 1000)
# Baseflow
gaugeBase <- integrate(approxfun(gauge_dat$mins, gauge_dat$baseflow), 
                       lower = 0, upper = max(gauge_dat$mins), subdivisions = 1000)

gaugePercentage <-
  gaugeBase$value / gaugeTotal$value


################################################################################
# Output variables
################################################################################
# Percentage baseflow for
# SWOT spatial hydrograph
swotPercentage

# Gauge temporal hydrograph.
gaugePercentage