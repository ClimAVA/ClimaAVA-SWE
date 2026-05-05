########################################
# Author - Sajad Khoshnood & Andre Moraes

# Utah State University
# QCNR - Watershed Sciences Department
# sajad.khoshnoodmotlagh@usu.edu & andre.moraes@usu.edu 

# This script is used to subset cmip6 models.

############ MPI-ESM-2-HR ####################

#loading files necessary to guide the loops
future_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/future_dates_2025.csv") # all dates for ssp245
historical_dates <- read.csv("/scratch/general/vast/u1080428/excel_files/historical_dates_2025.csv") # all dates for historical
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- "snw"

# Defining the range of days for historical and future SSP data
historical_days <- seq(1,12410,1)
ssp_days <- seq(12411,43800,1)

# Loading required libraries for netCDF manipulation, data manipulation, and database handling
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

####### SUBSETTING m = 5 ###################################----
####################################################################
m=5  # Setting the model index (11th model)

# Model-specific information: resolution of the grid
lon_res <- 0.937500 ###### Changes for every model
lat_res <- 0.937500

# Select the model (m = 7 for example)
model = models[m,1]
realization = models[m,5]
grid = models[m,6]

# Reading the guide file for the model, which contains geographic coordinates

guide <- read.dbf(paste0("//uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/",model,"_guide.dbf"))
#guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)


# Looping through each SSP scenario
#s=2
for (s in 1 :4){
  ssp = ssps[s] # Current SSP scenario
  print(ssp) # Print SSP scenario
  
  # Define the number of files based on the scenario (historical or future)
  if(ssp == "historical") {dates_n = models[m,7]}  # Historical data count
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates_n = models[m,8]}  # Future scenario data count
  
  # Set the corresponding date vector based on the scenario
  if(ssp == "historical") {dates = historical_dates[1:dates_n,m]}  # Historical dates
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates = future_dates[1:dates_n,m]}  # Future dates
  
  # Looping through each variable (v = 2 for example)
  #v=1
  for (v in 1){
    var = vars[v] # Selecting the variable
    print(var) # Print the variable
    
    # Subsetting the historical data
    if(ssp == "historical") {
      for (d in 1:length(dates)){ # Looping through all dates for this scenario
        date = dates[d] # Current date
        print(date) # Print the date
        
        # Building the path for the netCDF file
        nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
        
        # Open the netCDF file
        nc <- nc_open(nc_name)
        
        # Reading the variable data from the netCDF file
        array <- ncvar_get(nc, var)
        
        # Preparing an empty vector to store the pixel data for this date
        pixels = rep(NA, 1825)
        
        # Looping through the guide to extract specific pixels
        #p=224
        for (p in 1:length(guide$lon)){
          in_out <- guide[p,4] # Checking if the pixel is valid (in or out)
          
          # If pixel is out, assign NA
          if(in_out == 0){pixel = rep(NA, 365)}
          if(in_out == 1){ # If pixel is valid, extract data
            
            X <- (guide[p,2]/ lon_res)+1 # Calculate the X index for longitude
            Y <- ((guide[p,3] + 90)/lat_res)+1 # Calculate the Y index for latitude
            
            
            # Extracting the pixel data from the netCDF file
            if (d==1) {pixel <- array[X,Y, 366:1825]} # For the first date, use a specific slice of the array
            else {pixel <- array[X,Y, 1:1825]} # For subsequent dates, use another slice of the array
          }
          
          pixels <- cbind(pixels,pixel) # Adding the pixel data to the main vector
        }
        
        # Storing pixel data for each date
        if (d == 1) { pixels_d1 <- pixels[,-1] }
        if (d == 2) { pixels_d2 <- pixels[,-1] }
        if (d == 3) { pixels_d3 <- pixels[,-1] }
        if (d == 4) { pixels_d4 <- pixels[,-1] }
        if (d == 5) { pixels_d5 <- pixels[,-1] }
        if (d == 6) { pixels_d6 <- pixels[,-1] }
        if (d == 7) { pixels_d7 <- pixels[,-1] }
      }
      
      # Combine pixel data from all dates
      data <- rbind(pixels_d1, pixels_d2, pixels_d3, pixels_d4, pixels_d5, pixels_d6,
                    pixels_d7)
      
      # Creating the new netCDF file
      getwd() # Get current working directory
      setwd(paste0("/scratch/general/vast/u6055107/climava_swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data) # Convert pixel data to data frame
      rownames(data) <- as.character(1:length(data$pixel)) # Set row names
      colnames(data) <- as.character(1:length(data)) # Set column names
      data <- t(data) # Transpose the data frame
      
      # Defining the dimensions for the new netCDF file
      LON_n <- length(unique(guide$lon1)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 12410 # Time dimension for historical data
      
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n)) # Convert data to array
      
      # Defining netCDF dimensions and variables
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 19810101", vals = historical_days)
      
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  NA,longname = dim_long_name, prec = "double")
      
      # Creating the netCDF file and writing the data
      nc_out <- nc_create(nc_name,variable_dim)
      ncvar_put(nc_out, variable_dim, data_array)
      nc_close(nc_out) # Close the netCDF file
    }
    
    ##########################################################################
    
    # Handling future scenarios (ssp245, ssp370, ssp585)
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {
      #d=1
      for (d in 1:length(dates)){
        if (d == 1| d==2 | d== 3 | d == 4| d == 5| d == 6| d == 7 | d== 8 | d == 9 |
            d == 10 | d== 11 | d == 12 | d ==13 | d == 14 | d == 15 | d == 16 | d ==17){
          
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 1825)
          #p=1
          for (p in 1:length(guide$lon)){
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              
              X <- (guide[p,2]/ lon_res)+1
              Y <- ((guide[p,3] + 90)/lat_res)+1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
              
              pixel <- array[X,Y, 1:1825]} # change every model
            
            pixels <- cbind(pixels,pixel)}
          
          if (d == 1) { pixels_d1 <- pixels[,-1] }
          if (d == 2) { pixels_d2 <- pixels[,-1] }
          if (d == 3) { pixels_d3 <- pixels[,-1] }
          if (d == 4) { pixels_d4 <- pixels[,-1] }
          if (d == 5) { pixels_d5 <- pixels[,-1] }
          if (d == 6) { pixels_d6 <- pixels[,-1] }
          if (d == 7) { pixels_d7 <- pixels[,-1] }
          if (d == 8) { pixels_d8 <- pixels[,-1] }
          if (d == 9) { pixels_d9 <- pixels[,-1] }
          if (d == 10) { pixels_d10 <- pixels[,-1] }
          if (d == 11) { pixels_d11 <- pixels[,-1] }
          if (d == 12) { pixels_d12 <- pixels[,-1] }
          if (d == 13) { pixels_d13 <- pixels[,-1] }
          if (d == 14) { pixels_d14 <- pixels[,-1] }
          if (d == 15) { pixels_d15 <- pixels[,-1] }
          if (d == 16) { pixels_d16 <- pixels[,-1] }
          if (d == 17) { pixels_d17 <- pixels[,-1] }
        }
        
        if (d == 18){
          
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u1080428/snw/",model,"/",ssp,"/snw_day_",model,"_",ssp,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
          
          array <- ncvar_get(nc, var)
          
          pixels = rep(NA, 365)
          #p=1
          for (p in 1:length(guide$lon)){
            in_out <- guide[p,4]
            
            if(in_out == 0){pixel = rep(NA, 365)}
            if(in_out == 1){
              
              Y <- ((guide[p,3] + 90)/lat_res)+1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
              X <- (guide[p,4]/ lon_res)+1
              pixel <- array[X,Y, 1:365]} # change every model
            
            pixels <- cbind(pixels,pixel)}
          
          if (d == 18) { pixels_d18 <- pixels[,-1] }
        }
      }
      
      # Combine pixel data from all dates
      data <- rbind(pixels_d1, pixels_d2, pixels_d3, pixels_d4, pixels_d5, pixels_d6,
                    pixels_d7, pixels_d8, pixels_d9, pixels_d10, pixels_d11, pixels_d12,
                    pixels_d13, pixels_d14, pixels_d15, pixels_d16, pixels_d17, pixels_d18)
      
      # Creating the netCDF file
      getwd()
      setwd(paste0("/scratch/general/vast/u6055107/climava_swe/cmip6_data/subset/",model,"/",ssp,"/swe/"))  # Set output directory
      
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))
      colnames(data) <- as.character(1:length(data))
      data <- t(data)
      
      LON_n <- length(unique(guide$lon1)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 31390 # Time dimension for future data
      
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n)) # Convert data to array
      
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- "swe"
      dim_long_name <- "snow water equivalent"
      dim_units <- "mm"
      
      # Defining netCDF dimensions and variables
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 20150101", vals = ssp_days)
      
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  NA,longname = dim_long_name, prec = "double")
      
      # Creating the netCDF file and writing the data
      nc_out <- nc_create(nc_name,variable_dim)
      ncvar_put(nc_out, variable_dim, data_array)
      nc_close(nc_out) # Close the netCDF file
    }
  }
}

# End of script
