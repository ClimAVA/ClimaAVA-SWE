#########################################
# Author - Andre Moraes & Sajad Khoshnood
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu & sajad.khoshnoodmotlagh@usu.edu

# This script is used to downscaling GCM data.

################################################################
#packages
library(ncdf4)
library(dplyr)
library(randomForest)
library(Metrics)
library(doParallel)
library(spam)
library(foreign)
library(zoo)

# Load necessary files for guiding the loops

models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")

grid <- read.dbf("/scratch/general/vast/u6055107/climava_swe/shp_guides/swe_west_guide.dbf")
vars <- "swe"                        

files <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/files_validation_1992_2023.csv")

#########################################################################
#####Creating directories--------
# getwd()
# setwd("/scratch/general/vast/u6055107/climava_swe/validation/downscaling/1992_2023")
# 
# for (m in c(1,13)){ # models 2 and 17
#   model <- models[m,1]
#   print(model)
#   dir.create(model)
# 
#   }

####################################################################
#v=1
for (v in 1){ #downscaling all 3 variables
  
  var <- vars[v]
  print(var)
  
  #m=1
  for(m in c(1,13)) { #2 and 17
    
    
    model = models[m,1]
    print(model)
    
    
    guide <- read.dbf(paste0("/scratch/general/vast/u6055107/climava_swe/shp_guides/",model,"_guide.dbf"))
    #guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
    guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)
    
    
    
    guide_lat_res <- models[m,18] #taking lat resolution of each model basd on models file
    guide_lon_res <- models[m,19] #taking lon resolution of each model basd on models file
    guide_lat_length <- length(unique(guide$lat))
    guide_lon_length <- length(unique(guide$lon1))
    guide_lat <- unique(guide$lat)
    
    left <- min(guide$lon1) # Get the leftmost longitude for each model
    
    
    
    #loading our reference data
    nc_resampled <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/resample/",model,"_resample.nc"))
    
    resampled_array <- ncvar_get(nc_resampled, var)
    rm(nc_resampled)
    
    
    #f=3
    
    for (f in 1:3){ #it goes through years from 1981 to 2010
      
      #getting some information for the number of days and and starting and fishing year
      dates <- files[f,5]
      start <- files[f,3]
      finish <- files[f,4]
      length_1 <- files[f,2]
      SD <- files[f,6]
      print(Sys.time())
      print(dates)
      
      #directory to save the outputs
      #nc_name <- paste0("/uufs/chpc.utah.edu/common/home/moraes-group1/swe/training_model/validation_downscaling/",model,"/swe_validation_",dates,".nc")
      nc_name <- paste0("/scratch/general/vast/u6055107/climava_swe/validation/downscaling/1992_2023/",model,"/swe_validation_",model,"_",dates,".nc")
      
      #use conditional: if we have created the file do not create it again
      tem <- file.exists(nc_name)
      print(tem)
      
      if (tem == FALSE){
        
        ############################################################################################################
        #number of cores we asked to run the code
        registerDoParallel(7)
        
        #i=123089
        data <- foreach (i= 1:length(grid$id),  #1:96806 is the total number of pixel for the area of interest
                         .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "randomForest", "Metrics")) %dopar% {
                           
                           print(i)
                           IN_OUT <- grid[i,3]#identifying pixels located out or in of the border
                           
                           if (IN_OUT == 0){
                             
                             pixel <- rep(NA, length_1)
                             
                           }
                           
                           if (IN_OUT == 2){
                             
                             pixel <- rep(0, length_1)
                             
                           }
                           
                           
                           if (IN_OUT == 1){
                             
                             ID = as.integer(i)
                             print(ID)
                             # loading trained pixels
                             
                             model_name_up <- paste0("/scratch/general/vast/u6055107/climava_swe/validation/training/1992_2023/",model,"/rf_up_",ID,"_",model,"_",var,".rds")
                             model_name_down <-paste0("/scratch/general/vast/u6055107/climava_swe/validation/training/1992_2023/",model,"/rf_down_",ID,"_",model,"_",var,".rds")
                             
                             
                             #conditional: if we have any of files do not create it again
                             tem_up <-file.exists(model_name_up)
                             tem_down <-file.exists(model_name_down)
                             
                             
                             #tem <- all(file.exists(c(model_name_up,model_name_down)))
                             
                             
                             if (tem_up == TRUE | tem_down == TRUE){
                               
                               print(tem_up)
                               print(tem_down)
                               
                               
                               grid_lon = grid[i,4] #taking lon for each pixel
                               grid_lat = grid[i,5] #taking lat for each pixel
                               
                               
                               dist <- guide
                               #this used to identify the closest pixel 
                               dist$dist <- sqrt((dist$lon1-grid_lon)^2+(dist$lat-grid_lat)^2)
                               row <- dist[which.min(dist$dist),]
                               
                               # This all X and Y specifies how the method model should pass through the 9 pixels surrounding the first pixel.
                               X1 <- row[1,5] #lon
                               Y1 <- row[1,3] #lat
                               X2 <- row[1,5]
                               Y2 <- row[1,3] - guide_lat_res
                               X3 <- row[1,5] + guide_lat_res
                               Y3 <- row[1,3] - guide_lat_res
                               X4 <- row[1,5] + guide_lat_res
                               Y4 <- row[1,3]
                               X5 <- row[1,5] + guide_lat_res
                               Y5 <- row[1,3] + guide_lat_res
                               X6 <- row[1,5]
                               Y6 <- row[1,3] + guide_lat_res
                               X7 <- row[1,5] - guide_lat_res
                               Y7 <- row[1,3] + guide_lat_res
                               X8 <- row[1,5] - guide_lat_res
                               Y8 <- row[1,3]
                               X9 <- row[1,5] - guide_lat_res
                               Y9 <- row[1,3] - guide_lat_res
                               
                               # finding exact array position for each of the 9 pixels
                               #cov_1:9 > taking the values based on each pixel position for the whole period
                               
                               #1
                               
                               X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                               Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                               
                               
                               cov_1 <- resampled_array[X_1,Y_1,start:finish]
                               
                               #2
                               
                               X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                               Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_2 <- resampled_array[X_2,Y_2,start:finish]
                               
                               #3
                               
                               X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                               Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_3 <- resampled_array[X_3,Y_3,start:finish]
                               
                               #4
                               
                               X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                               Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_4 <- resampled_array[X_4,Y_4,start:finish]
                               
                               #5
                               
                               X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                               Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_5 <- resampled_array[X_5,Y_5,start:finish]
                               
                               #6
                               
                               X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                               Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_6 <- resampled_array[X_6,Y_6,start:finish]
                               
                               #7
                               
                               X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                               Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_7 <- resampled_array[X_7,Y_7,start:finish]
                               
                               #8
                               
                               X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                               Y_8 <- as.double(round(guide_lat_length - abs((Y8 - tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_8 <- resampled_array[X_8,Y_8,start:finish]
                               
                               #9
                               
                               X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                               Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                               
                               cov_9 <- resampled_array[X_9,Y_9,start:finish]
                               
                               
                               # creating a data frame depending variable and adding independent variables to it
                               cal <- as.data.frame(cov_1) # this cal is the observed data
                               
                               cal$cov_1 <- cov_1
                               cal$cov_2 <- cov_2
                               cal$cov_3 <- cov_3
                               cal$cov_4 <- cov_4
                               cal$cov_5 <- cov_5
                               cal$cov_6 <- cov_6
                               cal$cov_7 <- cov_7
                               cal$cov_8 <- cov_8
                               cal$cov_9 <- cov_9
                               
                               
                               cal <- cal[, colSums(is.na(cal)) == 0]
                               
                               
                               ##############################################################################
                               ##############################################################################
                               periods <- data.frame("cov_1" = coalesce(cal$cov_1, cal$cov_2, cal$cov_3,cal$cov_4, cal$cov_5, cal$cov_6,cal$cov_7, cal$cov_8, cal$cov_9))
                               
                               
                               # Initialize a new column for snow accumulation and snow melt periods
                               periods$period <- NA
                               
                               ######################################################################
                               
                               n_year <- files[f,7]
                               
                               for(n in 1:n_year){
                                 
                                 #defining peaks   
                                 if(n==1){
                                   peak_window_start <- 1
                                   peak_winodw_end <- 244
                                   peak_window_values <- periods$cov_1[peak_window_start:peak_winodw_end]
                                 }
                                 
                                 if(n!=1){
                                   peak_window_start <- (n-1)*365+1
                                   peak_winodw_end <- peak_window_start + 243
                                   peak_window_values <- periods$cov_1[peak_window_start:peak_winodw_end]
                                 }
                                 
                                 for(day in peak_window_start:peak_winodw_end){
                                   if (periods$cov_1[day] == max(peak_window_values)) {
                                     # Peak detected, last day of snow accumulation period
                                     periods$period[day+1] <- 2
                                   }
                                 }
                               }    
                               
                               
                               #defining bottoms 
                               for(n in 1:n_year){
                                 
                                 #defining bottoms  
                                 if(n==1){
                                   bottom_window_start <- 151
                                   bottom_winodw_end <- 355
                                   bottom_window_values <- periods$cov_1[bottom_window_start:bottom_winodw_end]
                                 }
                                 
                                 if(n!=1){
                                   bottom_window_start <- (n-1)*365+151
                                   bottom_winodw_end <- bottom_window_start + 204
                                   bottom_window_values <- periods$cov_1[bottom_window_start:bottom_winodw_end]
                                 }
                                 
                                 lowest_value <- min(bottom_window_values)
                                 
                                 for(day in bottom_window_start:bottom_winodw_end){
                                   
                                   vector <- periods$cov_1[(day):(day+9)]
                                   
                                   if(sum(vector) == (10*lowest_value)){
                                     periods$period[day+1] <- 1
                                     break
                                   }
                                 }
                               }    
                               
                               # Ensure the first growing season is filled with 0 instead of 1
                               if (is.na(periods$period[1])) {
                                 periods$period[1] <- 1
                               }
                               
                               # Fill forward the periods to mark the entire accumulation and melt periods
                               periods$period <- na.locf(periods$period, na.rm = FALSE, fromLast = FALSE)
                               
                               ###############################################################################
                               
                               cal$period <- periods$period
                               
                               ############################################################################
                               
                               ###############################################################################
                               cal$day <- 1:length(coalesce(cal$cov_1, cal$cov_2, cal$cov_3,cal$cov_4, cal$cov_5, cal$cov_6,cal$cov_7, cal$cov_8, cal$cov_9))
                               
                               cal_up <- cal[cal$period==1,]
                               cal_down <- cal[cal$period==2,]
                               
                               cal_up_days <- cal_up$day
                               cal_down_days <- cal_down$day
                               
                               
                               cal_up <- cal_up[, !names(cal_up) %in% c("period","day")]
                               cal_down <- cal_down[, !names(cal_down) %in% c("period","day")]
                               
                               rf_up <- readRDS(model_name_up)
                               cal_up$pred <- predict(rf_up,cal_up)
                               cal_up$day <- cal_up_days
                               
                               rf_down <- readRDS(model_name_down)
                               cal_down$pred <- predict(rf_down,cal_down)
                               cal_down$day <- cal_down_days
                               
                               cal_full <- rbind(cal_up,cal_down)
                               
                               cal_aranged <- arrange(cal_full,day)
                               
                               ############################################
                               
                               pixel <- as.vector(round(cal_aranged$pred,1))
                               
                               rm(rf_up,rf_down, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cov_7, cov_8, cov_9, cal,cal_full,
                                  cal_aranged ,cal_up,cal_down)
                               
                             } # if rf model exists
                           } # if in in loop
                           cbind(pixel)
                         } # for each loop
        
        # creating NetCDF
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data[,1]))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # defining dimensions
        LON_n <- length(unique(grid$lon)) 
        LAT_n <- length(unique(grid$lat))
        TIME_n <- length_1 
        
        
        # creating array
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        # naming dimensions
        dim_name <- "swe"
        dim_long_name <- "snow water equivalent"
        dim_units <- "mm"
        
        
        
        # defining dimensions
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(grid$lon))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(grid$lat))
        time_dim <- ncdim_def("time", units = "days", longname = paste0("days since ",SD), vals = seq(1,length_1,1))
        
        # variable dimensions
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  NA,longname = dim_long_name, prec = "double",compression = 9)
        
        # creating empty NetCDF 
        nc_out <- nc_create(nc_name,variable_dim)
        
        # adding variable to NetCDF
        ncvar_put(nc_out, variable_dim, data_array)
        
        
        # closing NetCDF
        nc_close(nc_out)
        
        rm(data, lon_dim, lat_dim,time_dim, variable_dim, nc_out)
        
        
        
      }
    }
  }  
}
