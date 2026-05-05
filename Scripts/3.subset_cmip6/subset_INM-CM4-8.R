########################################
# Author - Sajad Khoshnood & Andre Moraes

# Utah State University
# QCNR - Watershed Sciences Department
# sajad.khoshnoodmotlagh@usu.edu & andre.moraes@usu.edu 

# This script is used to subset cmip6 models.

############ INM-CM4-8 ####################
#loading required packages
library(ncdf4)  # Package for handling NetCDF files
library(dplyr)  # Package for data manipulation
library(foreign)  # Package for handling DBF files

#loading files necessary to guide the loops
future_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/future_dates_2025.csv") # all dates for ssp245
historical_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/historical_dates_2025.csv") # all dates for historical
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- "snw"

historical_days <- seq(1, 12410, 1)  # Number of days for historical data (1981-2014)
ssp_days <- seq(12411, 43800, 1)  # Number of days for future data (2015-2100)
#########################################################################
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
####### SUBSETING m = 3 ###################################----
####################################################################

#Model specific information

lon_res <- 2.00  ###### CHANGES FOR EVERY MODEL
lat_res <- 1.50   # Latitude and longitude resolution based on the model grid

##############################################################

m=3  # Setting the model index (11th model)

# Retrieving model, realization, and grid information
model = models[m,1]
realization = models[m,5]
grid = models[m,6]

# Reading the guide file for the model, which contains geographic coordinates

guide <- read.dbf(paste0("//uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/",model,"_guide.dbf"))
#guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)




# Loop over SSP scenarios (1-4)
#s=1
for (s in 1:4){
  ssp = ssps[s]  # Current scenario
  print(ssp)
  
  # Define the number of files based on the scenario (historical or future)
  if(ssp == "historical") {dates_n = models[m,7]}  # Historical data count
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates_n = models[m,8]}  # Future scenario data count
  
  # Set the corresponding date vector based on the scenario
  if(ssp == "historical") {dates = historical_dates[1:dates_n,m]}  # Historical dates
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates = future_dates[1:dates_n,m]}  # Future dates
  
  # Loop over variables (1-3)
  #v=1
  for (v in 1){
    var = vars[v]
    print(var)
    
    # Process for historical data
    if(ssp == "historical") {
      
      #d=1
      for (d in 1:length(dates)){
        
        if (d == 1){
          
          date = dates[d]  # Current date in the loop
          print(date)
          # Construct the file path for the NetCDF file
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)  # Open the NetCDF file
          
          # Collecting model data from the file
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 6935)  # Initialize an array to store pixel data
          #p=188
          
          for (p in 1:length(guide$lon)){
            
            in_out <- guide[p,4]  # Check if the pixel is inside the region
            
            if(in_out == 0){pixel = rep(NA, 365)}  # If outside, set pixel to NA
            if(in_out == 1){  # If inside, process pixel data
              
              X <- (guide[p,2]/ lon_res)+1  # Calculate X index
              Y <- ((guide[p,3] + 90)/lat_res)+1  # Calculate Y index
              
              pixel <- array[X,Y, 1:18250]}  # Extract data for the pixel
            
          
            #pixel <- tail(pixel,6935)  # Keep the last 6935 values
            pixels <- cbind(pixels,pixel)}  # Combine the data for all pixels
          
          if (d == 1) { pixels_d1 <- pixels[,-1] }  # Store data for the first date
        }
        
        if (d == 2){
          
          date = dates[d]  # Current date
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)  # Open the NetCDF file
          
          # Collect model data
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 5475)  # Initialize pixel data array
          #p=188
          for (p in 1:length(guide$lon)){
            
            in_out <- guide[p,4]  # Check if the pixel is inside the region
            
            if(in_out == 0){pixel = rep(NA, 365)}  # If outside, set to NA
            if(in_out == 1){  # If inside, process pixel data
              
              X <- (guide[p,2]/ lon_res)+1  # Calculate X index
              Y <- ((guide[p,3] + 90)/lat_res)+1  # Calculate Y index
              
              pixel <- array[X,Y, 1:5475]
            }  # Extract data for the pixel
            
           
            #pixel <- tail(pixel,5475)  # Keep the last 5475 values
            pixels <- cbind(pixels,pixel)}  # Combine pixel data
          
          if (d == 2) { pixels_d2 <- pixels[,-1] }  # Store data for the second date
        }
      }
      
      data <- rbind(pixels_d1, pixels_d2)  # Combine data for both dates
      
      # Creating the NetCDF file----
      
      getwd()
      setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)  # Convert data to a data frame
      rownames(data) <- as.character(1:length(data$pixel))  # Set row names
      colnames(data) <- as.character(1:length(data))  # Set column names
      data <- t(data)  # Transpose the data
      
      LON_n <- length(unique(guide$lon1))  # Number of unique longitudes
      LAT_n <- length(unique(guide$lat))  # Number of unique latitudes
      TIME_n <- 12410  # Number of time steps (historical)
      
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))  # Convert data to an array with appropriate dimensions
      
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      # Defining dimensions for the NetCDF file
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 19810101", vals = historical_days)
      
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  NA,longname = dim_long_name, prec = "double")  # Define the variable in the NetCDF file
      
      nc_out <- nc_create(nc_name,variable_dim)  # Create the NetCDF file
      
      ncvar_put(nc_out, variable_dim, data_array)  # Write data to the NetCDF file
      
      nc_close(nc_out)  # Close the NetCDF file
    }
    
    ##########################################################################
    
    # Process for future data (ssp245, ssp370, ssp585)
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {
      
      #d=2
      for (d in 1:length(dates)){
        
        if (d == 1){
          
          date = dates[d]  # Current date
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)  # Open the NetCDF file
          
          # Collect model data
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 18250)  # Initialize pixel data
          #p=146
          for (p in 1:length(guide$lon)){
            
            in_out <- guide[p,4]  # Check if pixel is inside the region
            
            if(in_out == 0){pixel = rep(NA, 365)}  # If outside, set to NA
            if(in_out == 1){  # If inside, process the pixel
              
              
              X <- (guide[p,2]/ lon_res)+1  # Calculate X index
              Y <- ((guide[p,3] + 90)/lat_res)+1  # Calculate Y index
              
              pixel <- array[X,Y, 1:18250]
            }  # Extract pixel data
            
           
            
            #pixel <- tail(pixel,18250)  # Keep last 18250 values
            pixels <- cbind(pixels,pixel)}  # Combine pixel data
          
          if (d == 1) { pixels_d1 <- pixels[,-1] }  # Store data for the first date
        }
        
        if (d == 2){
          
          date = dates[d]  # Current date
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)  # Open the NetCDF file
          
          # Collect model data
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 13140)  # Initialize pixel data array
          #p=146
          for (p in 1:length(guide$lon)){
            
            in_out <- guide[p,4]  # Check if pixel is inside the region
            
            if(in_out == 0){pixel = rep(NA, 365)}  # If outside, set to NA
            if(in_out == 1){  # If inside, process the pixel
              
              X <- (guide[p,2]/ lon_res)+1  # Calculate X index
              Y <- ((guide[p,3] + 90)/lat_res)+1  # Calculate Y index
              
              pixel <- array[X,Y, 1:13140]
            }  # Extract pixel data
            
           
            #pixel <- tail(pixel,13140)  # Keep last 13140 values
            pixels <- cbind(pixels,pixel)}  # Combine pixel data
          
          if (d == 2) { pixels_d2 <- pixels[,-1] }  # Store data for the second date
        }
      }
      
      data <- rbind(pixels_d1, pixels_d2)  # Combine data for both dates
      
      # Creating the NetCDF file----
      
      getwd()
      setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)  # Convert to data frame
      rownames(data) <- as.character(1:length(data$pixel))  # Set row names
      colnames(data) <- as.character(1:length(data))  # Set column names
      data <- t(data)  # Transpose the data
      
      LON_n <- length(unique(guide$lon1))  # Number of unique longitudes
      LAT_n <- length(unique(guide$lat))  # Number of unique latitudes
      TIME_n <- 31390  # Number of time steps (future)
      
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))  # Convert data to an array
      
      # Defining the NetCDF file name
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      
      # Defining dimensions for the NetCDF file
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 20150101", vals = ssp_days)
      
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  NA,longname = dim_long_name, prec = "double")  # Define variable
      
      nc_out <- nc_create(nc_name,variable_dim)  # Create the NetCDF file
      
      ncvar_put(nc_out, variable_dim, data_array)  # Write data to the NetCDF file
      
      nc_close(nc_out)  # Close the NetCDF file
    }
  }
}
