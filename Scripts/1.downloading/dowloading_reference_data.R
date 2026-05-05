########################################
# Script: download_snow_data.R
# Purpose: Download annual SWE (Snow Water Equivalent) and snow depth data 
#           from the NSIDC (National Snow and Ice Data Center) archive.
# Author: Sajad Khoshnood
# Date: [Add Date]
# Notes:
#   - Downloads NetCDF (.nc) files for each water year (1982–2023)
########################################

# Load required package for handling URLs
library(curl)

# Define the range of water years to download
years <- 1982:2023

# Check the current timeout setting for downloads
getOption("timeout")

# Increase timeout to prevent interruptions on slow connections
options(timeout = 10000)

#########################################################################
# Initialize loop indices (example placeholders)
d = 1
v = 2

# Loop through each year in the defined range
for (d in 1:length(years)) {
  
  # Select the current year
  year <- years[d]
  print(year)
  
  # Construct the NSIDC data URL for the given water year
  url <- paste0(
    "https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0719_SWE_Snow_Depth_v1/",
    "4km_SWE_Depth_WY", year, "_v01.nc"
  )
  
  # Define local output path for saving downloaded file
  dataset <- paste0(
    "/scratch/general/vast/u6055107/snow_validation_proposal/raw_data/SWE_Depth_",
    year, ".nc"
  )
  
  # Check if the file already exists locally
  file <- file.exists(dataset)
  
  # If the file does not exist, download it
  if (file == FALSE) {
    download.file(url, dataset, mode = "wb")  # mode="wb" ensures binary download
  }
}

########################################
# End of script
# Result: All missing SWE_Depth_<year>.nc files downloaded to target directory.
########################################
