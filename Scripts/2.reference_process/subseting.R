library(ncdf4)   # to read Netcdf files (*.nc)
library(raster)
library(tidyterra)
library(sf)
library(sp)
library(terra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(foreign)
library(abind)
library(doParallel)

years <- 1982:2023

# Register the parallel backend with 100 cores
registerDoParallel(8)

#v=2
#s=1
#y=1
#e=1
#b=4


#res <- 0.0625 # resolution of both data sets,


guide <- read.dbf("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/swe_west_expended_guide_in_out.dbf")

    
for (y in 1:12){
  year <- years[y]
  print(year)
  # create an empty list
  results_list <- list() 
  
  # input data from each scenarion 
  
  
  input_data <- nc_open(paste0("/scratch/general/vast/u6055107/snow_validation_proposal/raw_data/4km_SWE_Depth_WY",year,"_v01.nc")) # loading observed data
  
  
  # extract values for temperature
  
  var1 <- ncvar_get(input_data,"SWE")
  # extract lon and lat of the region
  lon <- ncvar_get(input_data,"lon")
  lat <- ncvar_get(input_data,"lat")
  time <- ncvar_get(input_data,"time")
  
  
  res_lon <- lon[2]-lon[1]
  res_lat <- lat[2]-lat[1]
  
  nc_out_name <- paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/reference_data/subset/swe_subset_",year,"_expanded.nc")
  
  
  exists <- file.exists(nc_out_name)
  
  if(exists == F){
    
    
    #i=233802
    results_list <- foreach(i=1:length(guide$id), .combine=rbind, .multicombine = TRUE,
                            .packages = c("doParallel", "foreach", "ncdf4", "dplyr")) %dopar% {
                              # Extract longitude and latitude for each pixel from 'guide' dataframe
                              X <- guide[i, 4] # Longitude for each pixel
                              Y <- guide[i, 5] # Latitude for each pixel
                              
                              # Determine if the pixel is inside (1) or outside (0) California
                              in_out <- guide[i, 2]
                              
                              if (in_out == 0) {
                                pixel <- rep(NA, length(time))
                              }
                              
                              # Process the pixel if it is inside 
                              if (in_out == 1) {
                                # Calculate the exact position of each lon and lat in the nc file
                                
                                ob_x <- ((-lon[1] + X)/res_lon)+1.1 
                                #ob_x <- (-(234.5312 - X) / res) + 1.1 # X pixel position in the observed dataset
                                ob_y <- as.double(length(lat) + ((Y - tail(lat, 1)) / res_lat))+0.1 # Y pixel position in the observed dataset
                                
                                # Extract the data for the pixel across all time points
                                pixel <- var1[ob_x, ob_y, 1:length(time)]
                              }
                              
                              # Combine the 'pixel' data with the main 'data' dataframe
                              return(pixel)
                              #rbind(pixel)
                            }
    
    
    ######### creating a new nc file for each file
   
    # Defining dimension values
    lon <- ncvar_get(input_data, "lon")
    lat <- ncvar_get(input_data, "lat")
    time <- 1:length(ncvar_get(input_data, "time"))
    
    LON_n <- length(unique(guide$lon))
    LAT_n <- length(unique(guide$lat))
    TIME_n <- length(time)
    
    
    # Dimension names
    dim_name <- "swe"
    dim_long_name <- "Snow Water Equivalent"
    dim_units <- "mm"
    dim_standard_name <- "millimeters h20"
    
    
    ##defining dimensions
    data_array <- array(results_list, dim = c(LON_n, LAT_n, TIME_n))
    
    lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon))
    lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
    time_dim <- ncdim_def("time", units = "days", longname = paste0("days since",year), vals = time)
    
    variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                              missval =  NA,longname = dim_long_name, prec = "double")
    
    nc_out <- nc_create(nc_out_name,variable_dim)
    
    ncvar_put(nc_out, variable_dim, data_array)  
    
    nc_close(nc_out)
    
    rm(var1)
    gc()
    print("done")
    
  }
  
}





