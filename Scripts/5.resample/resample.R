######################################################################
# Author - Sajad Khoshnood and Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
#  sajad.khoshnoodmotlagh@usu.edu & andre.moraes@usu.edu

# This script is used to create NetCDF files of resampled Reference data. Data is resampled to match pixel location and 
# spatial resolution of selected GCMs (14 GCMs)

#######################################################################
#setting the directory where files will be saved
# setwd("/scratch/general/vast/u6055107/snow_validation_proposal/resample")
# getwd()

#loading packages
library(doParallel)
library(foreign)
library(ncdf4)

#loading files necessary to guide the loops
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")


#do par parameters
registerDoParallel(3)



# load all ncs per variable
nc1 <- nc_open(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/reference_data/stacked/swe_west_stack_1982-1991.nc"))
nc2 <- nc_open(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/reference_data/stacked/swe_west_stack_1992-2001.nc"))
nc3 <- nc_open(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/reference_data/stacked/swe_west_stack_2002-2011.nc"))
nc4 <- nc_open(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/reference_data/stacked/swe_west_stack_2012-2023.nc"))


#extracting lat, lon dimensions and resolution from ncs
lon <- unique(ncvar_get(nc1, "lon")) # all longitudes
lon_res <- abs(lon[1] - lon[2]) # lon resolution
lat_lenght <- length(ncvar_get(nc1, "lat")) 
lat <- unique(ncvar_get(nc1, "lat")) # all latitudes
lat_res <- abs(lat[1] - lat[2])  # lat resolution

#Creating arrays
array1 <- ncvar_get(nc1, "swe")
array2 <- ncvar_get(nc2, "swe")
array3 <- ncvar_get(nc3, "swe")
array4 <- ncvar_get(nc4, "swe")


#removing unnecessary NCs
rm(nc1, nc2,nc3,nc4)


for(m in 1:14){
  
  model <- models[m,1]
  print(model)
  
  guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/",model,"_guide.dbf"))
  #guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
  guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)
  
 
  guide_lat_res <- models[m,18] #lat resolution of GCM
  guide_lon_res <- models[m,19] #lon resolution of GCM
  lat_n_pixels <- round(guide_lat_res / lat_res, 0) #number of pixels in lat
  lon_n_pixels <- round(guide_lon_res / lon_res, 0) #number of pixels in lon
  
  coarse_pixel <- rep(NA, 15340) # dummy object to be filled inside the loop
  
  #p=18
  for (p in 1:length(guide[,1])){ #for every pixel from the coarse file #####
    
    print(p)
    in_out <- guide[p,4]
    
    pixel <- if (in_out == 0) {rep(NA, 15340)} # 15705 is the number of days for the whole period 
    
    if (in_out == 1){
      guide_lon <- guide[p,5] # get central coordinates (lon flip)
      guide_lat <- guide[p,3] # get central coordinates
      
      #X <- round((guide_lon - lon[1]) / lon_res,0) + 1 # finding central coordinate in prism nc coordinates
      #Y <- as.double(round(lat_lenght - (guide_lat - tail(lat,1))/lat_res)) # finding central coordinate in prism nc coordinates
      
      X_list <- seq(guide_lon -( models[2,19]), guide_lon + ( models[2,19]),lon_res) # Creating a grid of all prism pixels inside the course pixel
      Y_list <- seq(guide_lat -(0.64 * models[2,18]), guide_lat + (0.66 * models[2,18]),lat_res)
      
      X_Y_list <- expand.grid(X_list, Y_list)
      #c=1
      # Getting all pixels from prism (fine) that are inside each GCM pixel (coarse)
      pixel <- foreach(c = 1:length(X_Y_list[,1]), .combine=cbind, .multicombine = T, ##
                       .packages = c("ncdf4")) %dopar% {
                         
                         #calculating the exact position of each pixel based on number of row and colmun in the nc file                
                         X_ <- abs(round((X_Y_list[c,1] - lon[1]) / lon_res,0) + 1) 
                         Y_ <- as.double(round(lat_lenght + (X_Y_list[c,2] - tail(lat,1))/lat_res))
                         
                         #based on input ncs time period the number of days will changes
                         pixel_1 <- array1[X_,Y_,1:3652]
                         pixel_2 <- array2[X_,Y_,1:3653]
                         pixel_3 <- array3[X_,Y_,1:3652]
                         pixel_4 <- array4[X_,Y_,1:4383]
                         
                         #putting all data together as one vector
                         pixel_ <- c(pixel_1, pixel_2,pixel_3,pixel_4)
                         
                         cbind(pixel_)
                         
                       }
      
      # averaging values
      pixel <- as.data.frame(pixel)
      pixel <- rowMeans(pixel, na.rm = TRUE)
    }
    
    coarse_pixel <- cbind(coarse_pixel, pixel)
    
  }
  
  print("creating NetCDF")
  print(Sys.time())
  #Organizing "data" to fit it in array
  data <- as.data.frame(coarse_pixel[,-1])
  rownames(data) <- as.character(1:length(data[,1])) #naming rows
  colnames(data) <- as.character(1:length(data)) #naming columns
  data <- t(data) # transposing data
  
  #defining dimensions for the array
  LON_n <- length(unique(guide$lon1)) 
  LAT_n <- length(unique(guide$lat))
  TIME_n <- 15340
  
  #creating the Array
  data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
  
  #naming dimension
  nc_name <- paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/resample/",model,"_resample.nc")
  dim_name <- "swe"
  dim_long_name <- "snow water equivalent"
  dim_units <- "mm"
  
  ##defining dimensions
  lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon1))
  lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
  time_dim <- ncdim_def("time", units = "days", longname = "days since 19820101", vals = seq(1,15340,1))
  
  #defining variable and compression
  variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                            missval =  NA,longname = dim_long_name, prec = "double")
  
  #creating empty NetCDF
  nc_out <- nc_create(nc_name,variable_dim)
  
  #adding variable to NetCDF
  ncvar_put(nc_out, variable_dim, data_array)
  
  
  #closing NetCDF
  nc_close(nc_out)
  gc()

}

#End of Script






