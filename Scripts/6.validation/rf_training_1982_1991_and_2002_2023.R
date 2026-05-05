#########################################
# Author - Andre Moraes
# Updated by Sajad Khoshnood
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu & sajad.khoshnoodmotlagh@usu.edu

# This script is used to create RF models to be used in downscaling GCM data

################################################################

#loading required packages
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

#########################################################################
# #####Creating directories--------
# getwd()
# setwd("/scratch/general/vast/u6055107/climava_swe/validation/training/1982_1991_and_2002_2023")
# 
# for (m in c(1,13)){ # models 2 and 17
#   model <- models[m,1]
#   print(model)
#   dir.create(model)
# 
#   }
####################################################################
#v=1
for (v in 1){ #trains all 3 variables
  
  var <- vars[v]
  print(var)
  start_time <- Sys.time()
  
  #m=8
  for(m in c(1,13)) { #2 and 17 # validation is for the finer (1) and (coarser) GCMs
    
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
    
    #loading our reference data(prism)
    nc_resampled <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/resample/",model,"_resample.nc"))
    
    resampled_array <- ncvar_get(nc_resampled, var)
    rm(nc_resampled)
    
    # loading only 30 years of data #
    nc1 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_1982-1991.nc"))
    nc2 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_2002-2011.nc"))
    nc3 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_2012-2023.nc"))
  
    
    lon <- unique(ncvar_get(nc1, "lon")) # all longitudes
    lon_res <- abs(lon[1] - lon[2]) # lon resolution
    lat_lenght <- length(ncvar_get(nc1, "lat")) 
    lat <- unique(ncvar_get(nc1, "lat")) # all latitudes
    lat_res <- abs(lat[1] - lat[2]) # lat resolution
    
    left <- min(guide$lon1) # Get the leftmost longitude for each model
    
    # extracting values of each year section
    array1 <- ncvar_get(nc1, var)
    array2 <- ncvar_get(nc2, var)
    array3 <- ncvar_get(nc3, var)
    
    
    
    rm(nc1,nc2,nc3)
    
    #number of cores to do the it parallel
    registerDoParallel(26)
    #i=58066
    
    foreach (i = 1:233022,  #foreach package used because of parallel process
             .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "randomForest", "Metrics")) %dopar% {
               
               IN_OUT <- grid[i,2] #identifying pixels located out or in of the border
               
               
               
               if (IN_OUT == 1){
                 
                 ID = as.integer(i)
                 print(ID)
                 # directory to save the outputs
                 model_name_up <- paste0("/scratch/general/vast/u6055107/climava_swe/validation/training/1982_1991_and_2002_2023/",model,"/rf_up_",ID,"_",model,"_",var,".rds")
                 model_name_down <-paste0("/scratch/general/vast/u6055107/climava_swe/validation/training/1982_1991_and_2002_2023/",model,"/rf_down_",ID,"_",model,"_",var,".rds")
                 
                 #conditional: if we have any of files do not create it again
                 tem_up <-file.exists(model_name_up)
                 tem_down <-file.exists(model_name_down)
                 
                 
                 #tem <- all(file.exists(c(model_name_up,model_name_down)))
                 
                 
                 if (tem_up == FALSE | tem_down == FALSE){
                   
                   
                   grid_lon = grid[i,4] #taking lon for each pixel
                   grid_lat = grid[i,5] #taking lat for each pixel
                   
                   
                   dist <- guide
                   #this used to identify from winch pixel should start to train (as number 1)
                   dist$dist <- sqrt((dist$lon1-grid_lon)^2+(dist$lat-grid_lat)^2)
                   row <- dist[which.min(dist$dist),]
                   
                   # This all X and Y specifies how the training model should pass through the 8 pixels surrounding the first pixel.
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
                   
                   #taking the exact position(x and y array position) of each pixel from reference file
                   X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                   Y_var <- as.double(round(lat_lenght + (grid_lat - tail(lat,1))/lat_res))
                   
                   # taking the values from the years we aim to train (1981-2010)
                   var_1 <- array1[X_var,Y_var,1:3652]
                   var_2 <- array2[X_var,Y_var,1:3652]
                   var_3 <- array3[X_var,Y_var,1:4383]
                   
                   
                   
                   #put all data together as one vector
                   VAR <- c(var_1, var_2,var_3)
                   
                   
                   # same process of finding exact array position for the remaining 8 pixels
                   #cov_1:9 > taking the values based on each pixel position for the whole peroid
                   #1
                   X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                   Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_1 <- resampled_array[X_1,Y_1,c(1:3652,7306:15340)] 
                   
                   #2
                   X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                   Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_2 <- resampled_array[X_2,Y_2,c(1:3652,7306:15340)]
                   
                   #3
                   X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                   Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_3 <- resampled_array[X_3,Y_3,c(1:3652,7306:15340)]
                   
                   #4
                   X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                   Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_4 <- resampled_array[X_4,Y_4,c(1:3652,7306:15340)]
                   
                   #5
                   X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                   Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_5 <- resampled_array[X_5,Y_5,c(1:3652,7306:15340)]
                   
                   #6
                   
                   X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                   Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_6 <- resampled_array[X_6,Y_6,c(1:3652,7306:15340)]
                   
                   #7
                   X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                   Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_7 <- resampled_array[X_7,Y_7,c(1:3652,7306:15340)]
                   
                   #8
                   
                   X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                   Y_8 <- as.double(round(guide_lat_length - abs((Y8 - tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_8 <- resampled_array[X_8,Y_8,c(1:3652,7306:15340)]
                   
                   #9
                   X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                   Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                   
                   cov_9 <- resampled_array[X_9,Y_9,c(1:3652,7306:15340)]
                   
                   
                   # creating a data frame depending variable and adding independent variables to it
                   cal <- as.data.frame(VAR) 
                   
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
                   ##############################################################################
                   periods <- data.frame("cov_1" = coalesce(cal$cov_1, cal$cov_2, cal$cov_3,cal$cov_4, cal$cov_5, cal$cov_6,cal$cov_7, cal$cov_8, cal$cov_9))
                   
                   
                   # Initialize a new column for snow accumulation and snow melt periods
                   periods$period <- NA
                   
                   ######################################################################
                   n_year <- 32
                   
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
                   
                   cal_up <- cal[cal$period == 1,]
                   cal_down <- cal[cal$period == 2,]
                   
                   cal_up <- cal_up[, !names(cal_up) %in% c("period")]
                   cal_down <- cal_down[, !names(cal_down) %in% c("period")]
                   
                   ############################################
                   
                   rf_model_up <- randomForest(formula = VAR ~ ., data = cal_up, ntree = 30)
                   #mean(rf_model_up$rsq)
                   #rf_model_up$importance
                   
                   rf_model_down <- randomForest(formula = VAR ~ ., data = cal_down, ntree = 30)
                   #mean(rf_model_down$rsq)
                   #rf_model_down$importance
                   
                   #save the traind model foe each pixel
                   saveRDS(rf_model_up, model_name_up)
                   saveRDS(rf_model_down, model_name_down)
                   
                   
                   
                   rm(rf_model_down,rf_model_up, grid_lon, grid_lat, dist, row, var_1, var_2,
                      VAR, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cov_7, cov_8, cov_9, cal,cal_up,cal_down)
                   gc()
                   
                   
                 }
               }
             }
  }
  end_time <- Sys.time()
  # Calculate the time difference
  time_taken <- end_time - start_time
}

