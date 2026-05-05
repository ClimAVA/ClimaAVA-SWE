#########################################
# Author - Sajad Khoshnood & Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# sajad.khoshnoodmotlagh@usu.edu & andre.moraes@usu.edu

# This script is used to bias correct GCM SWE data by Quantile Mapping. 
################################################################
# Loading required packages
library(sp)            # Spatial data handling
library(ncdf4)         # NetCDF file processing
library(dplyr)         # Data manipulation
library(randomForest)  # Machine learning methods
library(Metrics)       # Performance metrics
library(doParallel)    # Parallel processing
library(spam)          # Sparse matrix algebra
library(foreign)       # Reading .dbf files

### Set the working directory
setwd("/scratch/general/vast/u6055107/climava_swe/cmip6_data/bias_correction")
# Confirm the directory is set correctly.
getwd() 

# Set up parallel processing parameters
registerDoParallel(50)  # Register 50 cores for parallel computation

# Load necessary files for guiding the loops
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
vars <- "swe"  
# Define scenarios and variables for processing
ssps <- c("historical","ssp245","ssp370","ssp585")  # Scenarios: Historical and SSPs (future scenarios)


#################################
# Looping through variables and models
for (v in 1){ # Only process SWE.
  
  var <- vars[v]  # Extract variable name
  print(var)  # Display the variable being processed
  
  for(m in 1:14) { # Loop through all 14 models
    
    model = models[m,1]  # Extract model name
    print(model)  # Display the current model
    
    # Load the guide with pixel coordinates for the respective GCM
    
    
    guide <- read.dbf(paste0("/scratch/general/vast/u6055107/climava_swe/shp_guides_negative/",model,"_guide.dbf"))
    
    
    guide <- guide[,c(1,3,2,4)]  # Reorder columns for consistency
    
    realization <- models[m,5]  # Extract realization information (e.g., model ensemble member)
    
    # Open and read resampled Reference file for each model
    
    nc1 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/resample/",model,"_resample.nc")) 
    
    lon <- unique(ncvar_get(nc1, "lon"))  # Extract unique longitude values
    lon_res <- abs(lon[1] - lon[2])  # Calculate longitude resolution
    lat_lenght <- length(ncvar_get(nc1, "lat"))  # Get the number of latitude points
    lat <- unique(ncvar_get(nc1, "lat"))  # Extract unique latitude values
    lat_res <- abs(lat[1] - lat[2])  # Calculate latitude resolution
    
    reference_array <- ncvar_get(nc1, var)  # Extract Reference data as an array
    rm(nc1)  # Free memory
    
    # Open historical GCM data
    nc_hist <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/cmip6_data/subset/",model,"/historical/swe/snw_day_",model,"_",realization,"_historical_subset.nc")) 
    hist_array <- ncvar_get(nc_hist, var)  # Extract historical data
    rm(nc_hist)  # Free memory
    
    #s=1
    for (s in 1:4){  # Loop through scenarios
      
      ssp = ssps[s]  # Select the current scenario
      print(ssp)  # Display the scenario being processed
      
      # Open GCM data for the current scenario
      nc <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/cmip6_data/subset/",model,"/",ssp,"/swe/snw_day_",model,"_",realization,"_",ssp,"_subset.nc")) 
      to_bc_array <- ncvar_get(nc, var)  # Extract data to be bias-corrected
      
      if (ssp == "historical"){
        
        data <- foreach (i = 1: length(guide[,1]), .combine=cbind, .multicombine = T,
                         .packages = c("doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                           
                           # Extract pixel-specific coordinates
                           guide_lon <- guide[i,3]  # Longitude
                           guide_lat <- guide[i,4]  # Latitude
                           
                           # Calculate indices for Reference and GCM data
                           X <- round((guide_lon - lon[1]) / lon_res,0) + 1 
                           Y <- as.double(round(lat_lenght - abs((guide_lat - tail(lat,1))/lat_res))) 
                           
                           reference_vector <- reference_array[X,Y,1:12410]  # Extract Reference time series (1981-2014)
                          
                           
                           hist_vector <-  hist_array[X,Y,1:12410]  # Extract historical GCM time series (1981-2014)
                           hist_vector[hist_vector< 0] <- 0
                          
                           
                           if (is.na(reference_vector[1]) == TRUE) {
                             
                            pixel = rep(NA, 12410)  # Assign NA if Reference pixel is located outside of the study area border
                             
                           }
                           else{
                             
                             to_bc_vector <-  to_bc_array[X,Y,1:12410]  # Extract time series to be bias-corrected
                             to_bc_vector[to_bc_vector < 0] <- 0
                             
                             
                             if (any(hist_vector != 0)) {
                               hist_vector <- hist_vector[hist_vector != 0]
                             }
                             
                             if (any(reference_vector != 0)) {
                               reference_vector <- reference_vector[reference_vector != 0]
                             }
                             
                             
                             #hist_vector <- hist_vector[hist_vector != 0]
                             #reference_vector <- reference_vector[reference_vector != 0]
                             
                             # Calculate empirical cumulative distribution functions (ECDFs)
                             Reference_cdf <- ecdf(reference_vector)  # Reference CDF
                             hist_cdf <- ecdf(hist_vector)  # Historical GCM CDF
                            
                             # Perform quantile mapping
                             probability <- hist_cdf(to_bc_vector)  # Get probabilities from historical CDF
                             bc <- quantile(Reference_cdf, probability)  # Map probabilities to Reference CDF
                             data<- data.frame("gcm" = to_bc_vector,"bc" = bc)
                             data$bc[data$gcm == 0] <- 0
                             pixel <- data$bc
                             
                           }
                           
                           cbind(pixel)  # Combine results
                         }
        
        # Build the NetCDF output for historical scenario
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data[,1]))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # Define dimensions for NetCDF
        LON_n <- length(unique(guide$lon)) 
        LAT_n <- length(unique(guide$lat))
        
        TIME_n <- 12410 
        
        
        
        
        # Create data array
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        # Define metadata for the NetCDF
        nc_name <- paste0(ssp,"_",var,"_day_",model,"_BC.nc")
        # Dimension names
        dim_name <- "swe"
        dim_long_name <- "Snow Water Equivalent"
        dim_units <- "mm"
      
        # Define NetCDF dimensions
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
        time_dim <- ncdim_def("time", units = "days", longname = "days since start", vals = seq(1,12410,1))
        
        # Define variable dimensions
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  NA,longname = dim_long_name, prec = "double")
        
        # Create and populate the NetCDF file
        nc_out <- nc_create(nc_name,variable_dim)
        ncvar_put(nc_out, variable_dim, data_array)
        nc_close(nc_out)  # Close the NetCDF file
      }
      
      if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585"){
        
        data <- foreach (i = 1: length(guide[,1]), .combine=cbind, .multicombine = T,
                         .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                           
                           # Extract pixel-specific coordinates
                           guide_lon <- guide[i,3]
                           guide_lat <- guide[i,4]
                           
                           # Calculate indices for Reference and GCM data
                           X <- round((guide_lon - lon[1]) / lon_res,0) + 1 
                           Y <- as.double(round(lat_lenght - abs((guide_lat - tail(lat,1))/lat_res))) 
                           
                           
                           reference_vector <- reference_array[X,Y,1:12410]  # Extract Reference time series (1981-2014)
                           reference_vector[reference_vector < 0] <- 0
                           
                           
                           hist_vector <-  hist_array[X,Y,1:12410]  # Extract historical GCM time series (1981-2014)
                           hist_vector[hist_vector < 0] <- 0
                            
                         
                           if (is.na(reference_vector[1]) == TRUE) {
                             pixel = rep(NA, 31390)  # Assign NA if Reference pixel is located outside of the border
                             
                           }
                           else{
                             
                             to_bc_vector <-  to_bc_array[X,Y,1:31390]  # Future GCM data
                             to_bc_vector[to_bc_vector < 0] <- 0
                            
                             
                             if (any(hist_vector != 0)) {
                               hist_vector <- hist_vector[hist_vector != 0]
                             }
                             
                             if (any(reference_vector != 0)) {
                               reference_vector <- reference_vector[reference_vector != 0]
                             }
                             
                             
                             # Calculate ECDFs
                             Reference_cdf <- ecdf(reference_vector)  # Reference CDF
                             hist_cdf <- ecdf(hist_vector)  # Historical CDF
                             
                             # Perform quantile mapping
                             probability <- hist_cdf(to_bc_vector)  # Get probabilities from historical CDF
                             pixel <- quantile(Reference_cdf, probability)  # Map probabilities to Reference CDF
                           
                             # Perform quantile mapping
                             probability <- hist_cdf(to_bc_vector)  # Get probabilities from historical CDF
                             bc <- quantile(Reference_cdf, probability)  # Map probabilities to Reference CDF
                             data<- data.frame("gcm" = to_bc_vector,"bc" = bc)
                             data$bc[data$gcm == 0] <- 0
                             
                             pixel <- data$bc
                             
                             
                               
                           }
                           
                           cbind(pixel)  # Combine results
                         }
        
        # Build the NetCDF output for SSP scenarios
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data[,1]))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # Define dimensions for NetCDF
        LON_n <- length(unique(guide$lon)) 
        LAT_n <- length(unique(guide$lat))
        
        
        TIME_n <- 31390 
       
        
        # Create data array
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        # Define metadata for the NetCDF
        nc_name <- paste0(ssp,"_",var,"_day_",model,"_BC.nc")
        # Dimension names
        dim_name <- "swe"
        dim_long_name <- "Snow Water Equivalent"
        dim_units <- "mm"
        
        
        # Define NetCDF dimensions
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
        time_dim <- ncdim_def("time", units = "days", longname = "days since start", vals =seq(1,31390,1))
        
        # Define variable dimensions
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  NA,longname = dim_long_name, prec = "double")
        
        # Create and populate the NetCDF file
        nc_out <- nc_create(nc_name,variable_dim)
        ncvar_put(nc_out, variable_dim, data_array)
        nc_close(nc_out)  # Close the NetCDF file
      }
    }
  }
}
