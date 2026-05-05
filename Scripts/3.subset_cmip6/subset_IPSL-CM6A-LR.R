########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu
#
# This script is used to for downscaling GCM data

############ IPSL-CM6A-LR ####################

# Reading the input files
future_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/future_dates_2025.csv") # all dates for ssp245
historical_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/historical_dates_2025.csv") # all dates for historical
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- "snw"

# Defining the time periods for historical and future data
historical_days <- seq(1, 12410, 1)
ssp_days <- seq(12411, 43800, 1)

# Loading required libraries
library(ncdf4)   # For handling NetCDF files
library(dplyr)    # For data manipulation
library(foreign)  # For reading .dbf files (for model guides)

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

####### Subsetting model 8 (IPSL-CM6A-LR) ####################################
####################################################################

# Model-specific information (resolution and grid)
lon_res <- 2.5  ###### Longitude resolution (specific to the model)
lat_res <- 1.258741  ###### Latitude resolution (specific to the model)

# Defining model index (m = 14 for this example)
m = 8 

# Retrieving model, realization, and grid information
model = models[m,1]
realization = models[m,5]
grid = models[m,6]

# Reading the guide file for the model, which contains geographic coordinates

guide <- read.dbf(paste0("//uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/",model,"_guide.dbf"))
#guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)


# Looping over SSP scenarios
#s = 2
for (s in 2:4){
   # Setting the number of dates for the historical and future SSP scenarios
  ssp = ssps[s]  # Current scenario
  print(ssp)
  
  # Define the number of files based on the scenario (historical or future)
  if(ssp == "historical") {dates_n = models[m,7]}  # Historical data count
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates_n = models[m,8]}  # Future scenario data count
  
  # Set the corresponding date vector based on the scenario
  if(ssp == "historical") {dates = historical_dates[1:dates_n,m]}  # Historical dates
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates = future_dates[1:dates_n,m]}  # Future dates
  
  
  # Looping over variables
  #v = 1
  for (v in 1){
    var = vars[v]
    print(var)
    
    # Looping over each date in the selected range
    #d = 1
    if(ssp == "historical") {
      for (d in 1:length(dates)){
        
        date = dates[d]
        print(date)
        
        # Opening the NetCDF file for the selected model, scenario, variable, and date
        nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
        nc <- nc_open(nc_name)
        
        # Extracting data from the NetCDF file
        array <- ncvar_get(nc, var)
        
        # Initializing an empty vector for storing pixel values
        pixels = rep(NA, 12410)
        #p = 138
        for (p in 1:length(guide$lon)){
          in_out <- guide[p,4] # Checking if the pixel is inside the grid or not
          
          # Handling pixels outside the model grid
          if(in_out == 0) {pixel = rep(NA, 365)}
          if(in_out == 1){
            
            X <- (guide[p,2] / lon_res) + 1       # Longitude to pixel index conversion
            Y <- ((guide[p,3] + 90) / lat_res) + 1 # Latitude to pixel index conversion
            
            
            pixel <- array[X, Y, 1:60265]         # Extracting data for the pixel
            
          }
          
          # Removing leap years by excluding certain data points
          for (r in 1:40){ 
            remove  <- (r * 1460) + 1
            pixel <- pixel[-remove] # Removing leap years' extra days
          }
          pixel <- tail(pixel, 12410) # Keeping only the valid days
          pixels <- cbind(pixels, pixel) # Storing the pixel data
        }
        
        # Storing the pixel data for the first date
        if (d == 1) { pixels_d1 <- pixels[,-1] }
        
      }
      
      # Creating a data frame for the subsetted data
      data <- pixels_d1
      
      # Writing the subset data to a new NetCDF file
      getwd()
      setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))  # Row names for the data
      colnames(data) <- as.character(1:length(data))       # Column names for the data
      data <- t(data)  # Transposing the data frame to match the dimensions
      
      # Defining the dimensions for the new NetCDF file
      LON_n <- length(unique(guide$lon1)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 12410 # Number of time steps for historical data
      
      # Creating an array for the data
      data_array <- array(data, dim = c(LON_n, LAT_n, TIME_n))
      
      # Defining the NetCDF file name
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      # Defining the dimensions for longitude, latitude, and time
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 19810101", vals = historical_days)
      
      # Defining the variable in the NetCDF file
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval = NA, longname = dim_long_name, prec = "double")
      
      # Creating the NetCDF file and writing the data
      nc_out <- nc_create(nc_name, variable_dim)
      ncvar_put(nc_out, variable_dim, data_array)
      nc_close(nc_out)
    }
    
    # Repeat for future SSP scenarios
    if(ssp == "ssp245" | ssp == "ssp370" | ssp == "ssp585") {
      
      # Looping over dates for future SSP scenarios
      #d = 1
      for (d in 1:length(dates)){
        date = dates[d]
        print(date)
        
        # Opening the NetCDF file for the future scenario
        nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
        nc <- nc_open(nc_name)
        
        # Extracting data from the NetCDF file
        array <- ncvar_get(nc, var)
        
        # Initializing an empty vector for storing pixel values
        pixels = rep(NA, 31390)
        #p = 1
        for (p in 1:length(guide$lon)){
          in_out <- guide[p,4] # Checking if the pixel is inside the grid or not
          
          if(in_out == 0) {pixel = rep(NA, 365)} # Handling pixels outside the grid
          if(in_out == 1){
            #Y <- ((guide[p,3] + 90) / lat_res) + 1
            #X <- (guide[p,4] / lon_res) + 1
            
            X <- (guide[p,2] / lon_res) + 1       # Longitude to pixel index conversion
            Y <- ((guide[p,3] + 90) / lat_res) + 1 # Latitude to pixel index conversion
            
            pixel <- array[X, Y, 1:31411]  # Extracting data for the pixel
          }
          
          # Removing leap years for future scenarios
          for (r in 1:21){ # Adjust for leap years
            remove  <- (r * 1460) + 1
            pixel <- pixel[-remove]
          }
          pixel <- tail(pixel, 31390) # Keeping only valid data
          pixels <- cbind(pixels, pixel)
        }
        
        # Storing the pixel data for the first date
        if (d == 1) { pixels_d1 <- pixels[,-1] }
        
      }
      
      # Creating the data frame and writing to NetCDF file for future scenarios
      data <- pixels_d1
      
      getwd()
      setwd(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))
      colnames(data) <- as.character(1:length(data))
      data <- t(data)
      
      # Defining dimensions for the future scenario
      LON_n <- length(unique(guide$lon1)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 31390 # Number of time steps for future data
      
      # Creating the data array
      data_array <- array(data, dim = c(LON_n, LAT_n, TIME_n))
      
      
      # Defining the NetCDF file name
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
     
      # Defining the variable dimensions
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 20150101", vals = ssp_days)
      
      # Defining the variable in the NetCDF file
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval = NA, longname = dim_long_name, prec = "double")
      
      # Creating the NetCDF file and writing the data
      nc_out <- nc_create(nc_name, variable_dim)
      ncvar_put(nc_out, variable_dim, data_array)
      nc_close(nc_out)
      
    }
  }
}
# End of script
