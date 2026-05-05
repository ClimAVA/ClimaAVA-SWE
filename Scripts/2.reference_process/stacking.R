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




registerDoParallel(4)

y=1

years <- 1982:2023
# create an empty list
results_list <- list() 



for (y in 1:42){
  year <- years[y]
  print(year)
  
  # input data from each scenarion 
  nc1 <- nc_open(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/reference_data/subset/swe_subset_",year,"_expanded.nc")) # loading observed data
  
  # Combining files      
  data_var <- ncvar_get(nc1, "swe")
  
  
  results_list[[y]] <- data_var
  #nc_close(nc1)
  
}


data <- abind(results_list, along = 3)



nc_out_name <- paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/reference_data/stacked/swe_west_stack_2012-2023.nc")





######### creating a new nc file for each file


# Defining dimensions size

#Defining dimension values
lon <- ncvar_get(nc1, "lon")
lat <- ncvar_get(nc1, "lat")
time <- seq(1,4383,by=1)

LON_n <- length(unique(lon))
LAT_n <- length(unique(lat))
TIME_n <- 4383 #15340



# Dimension names
dim_name <- "swe"
dim_long_name <- "Snow Water Equivalent"
dim_units <- "mm"
dim_standard_name <- "millimeters h20"


##defining dimensions
data_array <- array(data, dim = c(LON_n, LAT_n, TIME_n))

lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(lon))
lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(lat))
time_dim <- ncdim_def("time", units = "days", longname = ("days since 2012"), vals = time)

variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                          missval =  NA,longname = dim_long_name, prec = "double")

nc_out <- nc_create(nc_out_name,variable_dim)

ncvar_put(nc_out, variable_dim, data_array)  

nc_close(nc_out)

gc()
print("done")








