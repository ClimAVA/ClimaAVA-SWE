########################################
# Author - Sajad Khoshnood & Andre Moraes

# Utah State University
# QCNR - Watershed Sciences Department
# sajad.khoshnoodmotlagh@usu.edu & andre.moraes@usu.edu 

# This script is used to subset cmip6 models.

############ MPI-ESM1-2-LR ####################

# Read in the CSV files for the future and historical dates, models, and variables
#loading files necessary to guide the loops
future_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/future_dates_2025.csv") # all dates for ssp245
historical_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/historical_dates_2025.csv") # all dates for historical
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- "snw"

# Define the day ranges for historical and future data
historical_days <- seq(1,12410,1)
ssp_days <- seq(12411,43800,1)

# Load necessary libraries
library(ncdf4)
library(dplyr)
library(foreign)

#########################################################################
#####Creating directories--------
# getwd()
# 
# 
# setwd("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset")
#  
# for (m in 1:15){
#   model = models[m,1]
#   dir.create(model)
#   
#   for (s in 1:4){
#         ssp = ssps[s]
#         dir = paste0(model,"/",ssp)
#         dir.create(dir)
#         
#         for (i in 1){
#                 v = vars[i]
#                 dir = paste0(model,"/",ssp,"/",v)
#                 dir.create(dir)
#         }
#  }
# } 
####################################################################
####### SUBSETTING m = 13 ###################################----
####################################################################

# Model specific information for GFDL-ESM4
lon_res <- 1.875000 ###### Changes for every model
lat_res <- 1.849638

# Set model index (m = 4 refers to the 4th model in the list)
m=13

# Retrieve the model information
model = models[m,1]
realization = models[m,5]
grid = models[m,6]

# Reading the guide file for the model, which contains geographic coordinates

guide <- read.dbf(paste0("//uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/",model,"_guide.dbf"))
#guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)

# Loop through each SSP scenario
#s=1
for (s in 1:4){
  ssp = ssps[s]  # Select SSP
  print(ssp)
  # Define number of dates based on SSP type
  if(ssp == "historical") {dates_n = models[m,7]}  # Historical data count
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates_n = models[m,8]}  # Future scenario data count
  
  # Set the corresponding date vector based on the scenario
  if(ssp == "historical") {dates = historical_dates[1:dates_n,m]}  # Historical dates
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates = future_dates[1:dates_n,m]}  # Future dates 
  
  # Loop through each variable (v = 2 refers to the second variable)
  #v=1
  for (v in 1){
    var = vars[v] # Select variable
    print(var)
    
    # If SSP is historical, process data for historical period
    if(ssp == "historical") {
      
      # Loop through each date
      #d=1
      for (d in 1:length(dates)){
        
        # Process first date (d = 1)
        if (d == 1){
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          # Collect model data (extract the variable array)
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 3285)  # Initialize pixels vector
          #p=168
          for (p in 1:length(guide$lon)){
            
            # Check if pixel is in or out of bounds
            in_out <- guide[p,4]
            
            # If pixel is out, set to NA
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              X <- (guide[p,2]/ lon_res)+1  # X position calculation
              Y <- ((guide[p,3] + 90)/lat_res)  # Y position calculation
              
              pixel <- array[X,Y, ]}  # Extract data for the pixel
            
            # Remove last day of leap year values
            for (r in 1:5){ 
              remove  <-  (r * 1460)+1
              pixel <- pixel[-remove]
            }
            
            pixel <- tail(pixel,3285)
            pixels <- cbind(pixels,pixel)}
          
          # Store pixels for the first date
          if (d == 1) { pixels_d1 <- pixels[,-1] }
        }
        
        # Process second date (d = 2)
        if (d == 2){
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          # Collect model data for the second date
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 7300)
          #p=168
          for (p in 1:length(guide$lon)){
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)
              
              pixel <- array[X,Y,]
            }
            
            # Remove last day of leap year values
            for (r in 1:5){ 
              remove  <-  (r * 1460)+1
              pixel <- pixel[-remove]
            }
            
            pixels <- cbind(pixels,pixel)}
          
          if (d == 2) { pixels_d2 <- pixels[,-1] }
        }
        
        # Process third date (d = 3)
        if (d == 3){
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 1825)
          #p=168
          for (p in 1:length(guide$lon)){
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)
              
              pixel <- array[X,Y, 1:1825]} 
            
         
            pixels <- cbind(pixels,pixel)}
          
          if (d == 3) { pixels_d3 <- pixels[,-1] }
        }
        
        # Combine the pixels for all dates
        data <- rbind(pixels_d1, pixels_d2, pixels_d3)
        
        # Save the data to a NetCDF file
        getwd()
        setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
        
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data$pixel))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # Define dimensions for NetCDF
        LON_n <- length(unique(guide$lon1)) 
        LAT_n <- length(unique(guide$lat))
        TIME_n <- 12410 
        
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        # Create NetCDF file
        nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
        dim_name <- "swe"
        dim_long_name <- "snow water equivalent"
        dim_units <- "mm"
        
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
        time_dim <- ncdim_def("time", units = "days", longname = "days since 19810101", vals = historical_days)
        
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  NA,longname = dim_long_name, prec = "double")
        
        nc_out <- nc_create(nc_name,variable_dim)
        
        ncvar_put(nc_out, variable_dim, data_array)
        
        nc_close(nc_out)
      }
    }
    
    # Process future dates (for SSPs 245, 370, and 585)
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {
      
      # Loop through each date for future projections
      #d=1
      for (d in 1:length(dates)){
        
        if (d == 1 | d == 2 | d == 3 | d == 4){
          
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 7300)
          #p=1
          for (p in 1:length(guide$lon)){
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)
              
              pixel <- array[X,Y, 1:7300]} 
            
            # Remove last day of leap year values
            for (r in 1:5){ 
              remove  <-  (r * 1460)+1
              pixel <- pixel[-remove]
            }
            
            
            pixels <- cbind(pixels,pixel)}
          
          if (d == 1) { pixels_d1 <- pixels[,-1] }
          if (d == 2) { pixels_d2 <- pixels[,-1] }
          if (d == 3) { pixels_d3 <- pixels[,-1] }
          if (d == 4) { pixels_d4 <- pixels[,-1] }
        }
        
        # Process fifth date (d = 5)
        if (d == 5){
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 2190)
          #p=1
          for (p in 1:length(guide$lon)){
            
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)
              
              pixel <- array[X,Y, 1:2190]} 
            
            # Remove last day of leap year values
            for (r in 1){ 
              remove  <-  (r * 1460)+1
              pixel <- pixel[-remove]
            }
            
            
            pixels <- cbind(pixels,pixel)}
          
          if (d == 5) { pixels_d5 <- pixels[,-1] }
        }
      }
      
      # Combine the pixels for all future dates
      data <- rbind(pixels_d1, pixels_d2, pixels_d3, pixels_d4, pixels_d5)
      
      # Create and save the NetCDF for future data
      getwd()
      setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))
      colnames(data) <- as.character(1:length(data))
      data <- t(data)
      
      # Define dimensions for the NetCDF file
      LON_n <- length(unique(guide$lon1)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 31390 
      
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
      
      # Create the NetCDF file for the future data
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      
      
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 20150101", vals = ssp_days)
      
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  NA,longname = dim_long_name, prec = "double")
      
      nc_out <- nc_create(nc_name,variable_dim)
      
      ncvar_put(nc_out, variable_dim, data_array)
      
      nc_close(nc_out)
    }
  }
}
