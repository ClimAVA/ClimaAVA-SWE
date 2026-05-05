########################################
# Author - Sajad Khoshnood & Andre Moraes

# Utah State University
# QCNR - Watershed Sciences Department
# sajad.khoshnoodmotlagh@usu.edu & andre.moraes@usu.edu 

# This script is used to subset cmip6 models.
############ MIROC6 ####################
# Reading the future dates for SSPs scenario from CSV file
future_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/future_dates_2025.csv") # all dates for ssp245
historical_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/historical_dates_2025.csv") # all dates for historical
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- "snw"

# Defining the time ranges for historical and future scenarios
historical_days <- seq(1,12410,1)
ssp_days <- seq(12411,43800,1)

# Loading required libraries
library(ncdf4)  # For handling NetCDF files
library(dplyr)   # For data manipulation
library(foreign) # For handling DBF files

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

####### Subsetting Data for Model m = 9 #############################

# Model-specific information
lon_res <- 1.406250  ###### Resolution change for each model
lat_res <- 1.406250  # Resolution change for each model

##############################################################
m=9  # Selecting model 16 (MIROC6)

model = models[m,1]
realization = models[m,5]
grid = models[m,6]

# Reading the guide file for the model, which contains geographic coordinates

guide <- read.dbf(paste0("//uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/",model,"_guide.dbf"))
#guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)



# Loop over the four SSP scenarios
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
  
  #v=1  # Looping through variables (e.g., temperature, precipitation)
  for (v in 1){
    var = vars[v]  # Current variable name
    print(var)  # Print current variable
    
    if(ssp == "historical") {
      
      # Loop over the dates for historical data
      for (d in 1:length(dates)){
        
        if (d == 1| d==2 | d== 3){
          date = dates[d]
          print(date)  # Print current date
          
          # Construct file path for the current NetCDF file
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)  # Open the NetCDF file
          
          array <- ncvar_get(nc, var)  # Read variable data from NetCDF file
          
          pixels = rep(NA, 3650)  # Initialize empty pixel array
          #p=200
          
          # Loop over each pixel in the guide and extract corresponding data
          for (p in 1:length(guide$lon)){
            in_out <- guide[p,4]  # Check if pixel is inside the region
            
            # Assign NA for pixels outside the region
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){  # Extract data for pixels inside the region
              
              # Calculate pixel positions based on latitude and longitude
              X <- (guide[p,2]/ lon_res)+1  # Longitude position
              Y <- ((guide[p,3] + 90)/lat_res)+1  # Latitude position
              
              
              # Extract the data for the current date and pixel
              if (d==1){pixel <- array[X,Y, 366:3650]}  # Adjust indexing for first date
              else {pixel <- array[X,Y, 1:3650]}  # Extract data for subsequent dates
            }
            
            # Append pixel data to the pixels array
            pixels <- cbind(pixels,pixel)
          }
          
          # Store data for each date
          if (d == 1) { pixels_d1 <- pixels[,-1] }
          if (d == 2) { pixels_d2 <- pixels[,-1] }
          if (d == 3) { pixels_d3 <- pixels[,-1] }
        }
        
        # Process the 4th date for the historical scenario
        if (d == 4){
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 1825)  # Initialize empty pixel array
          #p=1
          for (p in 1:length(guide$lon)){
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)+1
              
              pixel <- array[X,Y, 1:1825]}  # Extract data for the 4th date
            
            pixels <- cbind(pixels,pixel)}
          
          if (d == 4) { pixels_d4 <- pixels[,-1] }
        }
      }
      
      # Combine data for all historical dates
      data <- rbind(pixels_d1, pixels_d2, pixels_d3, pixels_d4)
      
      # Creating the new NetCDF file with the subsetted data
      getwd()  # Get current working directory
      setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)  # Convert data to a data frame
      rownames(data) <- as.character(1:length(data$pixel))  # Set row names
      colnames(data) <- as.character(1:length(data))  # Set column names
      data <- t(data)  # Transpose the data
      
      LON_n <- length(unique(guide$lon1))  # Number of unique longitudes
      LAT_n <- length(unique(guide$lat))  # Number of unique latitudes
      TIME_n <- 12410  # Number of time steps for historical data
      
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))  # Convert data to array for NetCDF
      
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      # Defining dimensions for the NetCDF file
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 19810101", vals = historical_days)
      
      # Define the variable within the NetCDF file
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  NA,longname = dim_long_name, prec = "double")
      
      # Create and write the NetCDF file
      nc_out <- nc_create(nc_name, variable_dim)
      ncvar_put(nc_out, variable_dim, data_array)
      nc_close(nc_out)  # Close the NetCDF file after writing
      
    }
    
    ##########################################################################
    
    # For future SSP scenarios (ssp245, ssp370, ssp585), the process is similar
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {
      
      #d=1
      for (d in 1:length(dates)){
        
        if (d == 1 |d==2 | d== 3 | d == 4| d == 5| d == 6| d == 7 | d== 8){
          
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 3650)
          #p=1
          for (p in 1:length(guide$lon)){
            
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)+1
              
              pixel <- array[X,Y, 1:3650]} 
            
            pixels <- cbind(pixels,pixel)}
          
          if (d == 1) { pixels_d1 <- pixels[,-1] }
          if (d == 2) { pixels_d2 <- pixels[,-1] }
          if (d == 3) { pixels_d3 <- pixels[,-1] }
          if (d == 4) { pixels_d4 <- pixels[,-1] }
          if (d == 5) { pixels_d5 <- pixels[,-1] }
          if (d == 6) { pixels_d6 <- pixels[,-1] }
          if (d == 7) { pixels_d7 <- pixels[,-1] }
          if (d == 8) { pixels_d8 <- pixels[,-1] }
          
        }
        
        if (d == 9) {
          
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 1825)
          p=1
          for (p in 1:length(guide$lon)){
            
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)+1
              
              pixel <- array[X,Y, 1:1825]} 
            
            pixels <- cbind(pixels,pixel)}
          
          
          if (d == 9) { pixels_d9 <- pixels[,-1] }
          
        }
      }
      
      # Combine all data for future scenarios
      data <- rbind(pixels_d1, pixels_d2, pixels_d3, pixels_d4, pixels_d5, pixels_d6,
                    pixels_d7, pixels_d8, pixels_d9)
      
      # Creating the NetCDF file for future scenarios
      getwd()
      setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))
      colnames(data) <- as.character(1:length(data))
      data <- t(data)
      
      LON_n <- length(unique(guide$lon1)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 31390  # Number of time steps for future data
      
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
      
      # Defining the NetCDF file name
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      
      # Defining dimensions for future data
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 20150101", vals = ssp_days)
      
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  NA,longname = dim_long_name, prec = "double")
      
      # Creating and writing the NetCDF file
      nc_out <- nc_create(nc_name, variable_dim)
      ncvar_put(nc_out, variable_dim, data_array)
      nc_close(nc_out)  # Close the NetCDF file after writing
      
    }
  }
}
