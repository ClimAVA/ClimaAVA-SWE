#########################################
# Author - Andre Moraes and Sajad Khoshnood
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu & sajad.khoshnoodmotlagh@usu.edu
#
# This script is used to for downscaling GCM data
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

#loading data frames used to guide the process
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
grid <- read.dbf("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/swe_west_guide.dbf")
vars <- "swe"                                
ssps <- c("historical","ssp245","ssp370","ssp585")

#########################################################################
####Creating directories--------
# 
# getwd()
# setwd("/scratch/general/vast/u6055107/climava_swe/future/downscaling")
# 
# for (m in 1:14){
#   model = models[m,1]
#   dir.create(model)
# 
#   for (s in 1:4) {
#     ssp <- ssps[s]
#     print(ssp)
# 
#     dir <- paste0(model,"/",ssp)
# 
#     dir.create(dir)
#  }
# }

####################################################################
# v=1
# s=1
# m=15
for (v in 1){# loop trough variables
  
  var <- vars[v]
  print(var)
  
  for (s in 1:4){ # loop through scenarios
    
    ssp = ssps[s]
    print(ssp)
    
   
    # loop trough models
    for(m in 1:14) { 
      
      model = models[m,1]
      print(model)
      realization <- models[m,5]
      
      
      
      #loading guides with files start and end date and other information
      if(ssp == "historical") {files <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/files_DS_hist.csv")}
      if(ssp %in% c("ssp245", "ssp370", "ssp585")) {files <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/files_DS_future.csv")}
      
      
    
      
      #asking for some information to run the process through loops
      if(ssp == "historical") {version = models[m,9]}
      if(ssp == "ssp245") {version = models[m,10]}
      if(ssp == "ssp370") {version = models[m,11]}
      if(ssp == "ssp485") {version = models[m,12]}
    
      
      guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/",model,"_guide.dbf"))
      #guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
      guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)
      
      guide_lat_res <- models[m,18] #taking lat resolution of each model based on models file
      guide_lon_res <- models[m,19] #taking lon resolution of each model based on models file
      guide_lat_length <- length(unique(guide$lat))
      guide_lon_length <- length(unique(guide$lon1))
      guide_lat <- unique(guide$lat)
     
      left <- min(guide$lon1) # Get the leftmost longitude for each model
      
      
      # loading bias corrected data  
      
      
      nc_bc <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/cmip6_data/bias_correction/",ssp,"_",var,"_day_",model,"_BC.nc"))
      
      bc_array <- ncvar_get(nc_bc, var)
      rm(nc_bc) 
      #f=1
      # loop through each file to be created
      for (f in 1: length(files[,1])){
        #getting some information for the number of days and and starting and fishing year
        dates <- files[f,5]
        start <- files[f,3]
        finish <- files[f,4]
        length_1 <- files[f,2]
        SD <- files[f,6]
        start_time <- print(Sys.time())   
        print(dates)
        
        
        data_since <- as.numeric(substr(dates,1,8))  # to take the first date of staring date for each period
        
        
        
        # defining the name of the file                                                                                                                                                                                        
        nc_name <- paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/",ssp,"/ClimAVA-SWE_west_",model,"_",ssp,"_",realization,"_",dates,".nc")
        
        # testing if file was already created
        tem <- file.exists(nc_name)
        print(tem)
        
        if (tem == FALSE){
          
          ############################################################################################################
          #i=204214
          
          # defining dopar parameters  
          registerDoParallel(6)
          
          # downscaling is done for each pixel
          data <- foreach (i =1:length(grid$id),
                           .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "randomForest", "Metrics")) %dopar% {
                             
                             #identifying pixels located out or in of the border
                             IN_OUT <- grid[i,3] 
                             
                             if (IN_OUT == 0){
                               
                               pixel <- rep(NA, length_1)
                               
                             }
                             
                             if (IN_OUT == 2){
                               
                               pixel <- rep(0, length_1)
                               
                             }
                             
                             if (IN_OUT == 1){
                               
                               ID = i
                               print(ID)
                               
                               # loading trained pixels
                               model_name_up <- paste0("/scratch/general/vast/u6055107/climava_swe/future/training1/",model,"/rf_up_",ID,"_",model,"_",var,".rds")
                               model_name_down <-paste0("/scratch/general/vast/u6055107/climava_swe/future/training1/",model,"/rf_down_",ID,"_",model,"_",var,".rds")
                               
                               
                               #conditional: if we have any of files do not create it again
                               tem_up <-file.exists(model_name_up)
                               tem_down <-file.exists(model_name_down)
                               
                               
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
                                 
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- bc_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- bc_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- bc_array[X_4,Y_4,start:finish]
                                 
                                 #5
                                 
                                 X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                                 Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_5 <- bc_array[X_5,Y_5,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- bc_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- bc_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- bc_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- bc_array[X_9,Y_9,start:finish]
                                 
                                 
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
                                 
                                 
                                 #cal <- cal[, colSums(is.na(cal)) == 0]
                                 #colnames(cal)[colSums(is.na(cal)) > 0]
                                
                                 
                                 ##############################################################################
                                 
                                 periods <- data.frame("cov_1" = coalesce(cal$cov_1, cal$cov_2, cal$cov_3,cal$cov_4, cal$cov_5, cal$cov_6,cal$cov_7, cal$cov_8, cal$cov_9))
                                 
                                 
                                 # Initialize a new column for snow accumulation and snow melt periods
                                 periods$period <- NA
                                 
                                 ######################################################################
                                 
                                 n_year <- length_1/365
                                 
                                 for(n in 1:n_year){
                                   
                                 #defining peaks   
                                  if(n==1){
                                    peak_window_start <- 1
                                    peak_winodw_end <- 182
                                    peak_window_values <- periods$cov_1[peak_window_start:peak_winodw_end]
                                  }
                                   
                                  if(n!=1){
                                   peak_window_start <- (n-1)*365+1
                                   peak_winodw_end <- peak_window_start + 181
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
                                     bottom_window_start <- 60
                                     bottom_winodw_end <- 355
                                     bottom_window_values <- periods$cov_1[bottom_window_start:bottom_winodw_end]
                                   }
                                   
                                   if(n!=1){
                                     bottom_window_start <- (n-1)*365+60
                                     bottom_winodw_end <- bottom_window_start + 295
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
                                 ###############################################################################
                                 
                                 cal$period <- periods$period
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
                                 cal_full$pred[cal_full$pred  < 0] <- 0
                                 cal_aranged <- arrange(cal_full,day)
                                 
                                 ############################################
                                 
                                 pixel <- as.vector(round(cal_aranged$pred,1))
                                 
                                 rm(rf_up,rf_down, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cov_7, cov_8, cov_9, cal,cal_full,
                                    cal_aranged ,cal_up,cal_down,periods)
                                 
                               
                               }
                             }
                             cbind(pixel)
                             
                           }
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
          
          # naming dimensions
          dim_name <- "swe"
          dim_long_name <- "Daily snow water equivalent"
          dim_units <- "kg.m-2.s-1"
          dim_standard_name <- "surface_snow_amount"
          
          
          # defining dimensions
          lon_dim <- ncdim_def("longitude", units = "degrees_east", longname = "Longitude", vals = unique(grid$lon))
          lat_dim <- ncdim_def("latitude", units = "degrees_north", longname = "Latitude", vals = unique(grid$lat))
          time_dim <- ncdim_def("time", units = "days", longname = paste0("days since ",data_since), vals = seq(1,length_1,1))
          
          
          # variable dimensions
          variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                    missval =  NA,longname = dim_long_name, prec = "double", compression = 9)
          
          # creating empty NetCDF 
          nc_out <- nc_create(nc_name,variable_dim)
          
          # adding variable to NetCDF
          ncvar_put(nc_out, variable_dim, data_array)
          
          
          
          ncatt_put(nc_out, var, "standard_name", dim_standard_name)
          ncatt_put(nc_out, 0, "Title", "Climate data for Adaptation and Vulnerability Assesments - Snow Water Equivalent (ClimAVA-SWE)","character")
          ncatt_put(nc_out, 0, "Conventions", "CF-1.0","character")
          ncatt_put(nc_out, 0, "Version", "01","character")
          ncatt_put(nc_out, 0, "Creation_date", paste0(Sys.time()),"character")
          ncatt_put(nc_out, 0, "Source", "Utah State University, Watershed Sciences Department","character")
          ncatt_put(nc_out, 0, "Address", "5210 Old Main Hill, NR 210, Logan, UT 84322","character")
          ncatt_put(nc_out, 0, "Creator", "Sajad Khoshnood motlagh, Andre Geraldo de Lima Moraes, Kayla Smith","character")
          ncatt_put(nc_out, 0, "Contact", "sajad.khoshnoodmotlagh@usu.edu & andre.moraes@usu.edu","character")
          ncatt_put(nc_out, 0, "Description", "The ClimAVA-SWE dataset provides high-resolution (4 km) bias-corrected, downscaled future climate projections based on 14 CMIP6 GCMs. The dataset includes the snow water equivalent (SWE) variable and three Shared Socioeconomic Pathways (SSP245, SSP370, SSP585) for the western United States. ClimAVA-SWE employs the Spatial Pattern Interaction Downscaling (SPID) method, which uses Random Forest models to capture relationships and interactions between coarse-resolution spatial patterns and fine-resolution pixel values. Two Random Forest models are trained per pixel—one for the accumulation period and one for the melting period—using high-resolution reference data as targets and nine neighboring pixels from spatially resampled (coarser) versions as predictors. These trained models are then applied to downscale the GCM data. For more details, refer to the publication describing the method and dataset.","character")
          ncatt_put(nc_out, 0, "Lineage", paste0("Reference SWE Data: We utilized the Daily Snow Water Equivalent, Version 1 dataset from the National Snow and Ice Data Center (NSIDC-0719) (https://nsidc.org/data/nsidc-0719/versions/1). This file contains data downscaled from model ", model, ", SSP ", ssp, ", variant label ", realization, ", version ", version, "."), "character")
          ncatt_put(nc_out, 0, "License", "CC-BY-SA 4.0","character")
          ncatt_put(nc_out, 0, "Fees", "This data set is free","character")
          ncatt_put(nc_out, 0, "Disclaimer", "While every effort has been made to ensure the accuracy and completeness of the data, no guarantee is given that the information provided is error-free or that the dataset will be suitable for any particular purpose. Users are advised to use this dataset with caution and to independently verify the data before making any decisions based on it. The creators of this dataset make no warranties, express or implied, regarding the dataset's accuracy, reliability, or fitness for a particular purpose. In no event shall the creators be liable for any damages, including but not limited to direct, indirect, incidental, special, or consequential damages, arising out of the use or inability to use the dataset. Users of this dataset are encouraged to properly cite the dataset in any publications or works that make use of the data. By using this dataset, you agree to these terms and conditions. If you do not agree with these terms, please do not use the dataset.","character")
          
          # closing NetCDF
          nc_close(nc_out)
          
          time_difference <- print((Sys.time()) -start_time)
          print(paste0(model,"/",var,"/",ssp," is finished"))
          
          rm(data, lon_dim, lat_dim,time_dim, variable_dim, nc_out)
          gc()
        } 
      }
    }
  }  
}



