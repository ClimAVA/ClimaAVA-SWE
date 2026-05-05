### this code is used to create a graph showing Reference, GCMs and Bias corrected data by line graph for each model
#packages ----
library(ncdf4)
library(doParallel)
library(foreign)
library(sp)
library(terra)
library(dplyr)
library(raster)
library(hydroGOF)
library(trend)
library(cowplot)
#Loading files



models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
study_area <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/shps/selsected_area_west.shp")

vars <- "swe"    

#models <- c("EC-Earth3","NorESM2-LM")
#dates <- c("20110101-20151231","20160101-20201231")
metrcis_average <- list()

#Starting loops ----
#m=12
for (m in 1:14){ #looping though models models 1:14
  
  model <- models[m,1] # geeing the names of the model from the models table
  print(model)
  realization <- models[m,5]
  
  
  guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/guides/",model,"_guide.dbf"))
  #guide$lon1 <- ifelse(guide$lon < 0, (360 + guide$lon), guide$lon)
  guide$lon1 <- ifelse(guide$lon > 0, (guide$lon - 360), guide&lon)
  
  
  # Convert guide coordinates to sf points
  guide_sf <- st_as_sf(guide, coords = c("lon1", "lat"), crs = st_crs(study_area))
  
  # Ensure same CRS
  guide_sf <- st_transform(guide_sf, crs = st_crs(study_area))
  
  # Spatial join to identify which guide points fall inside study_area
  guide_sf$in_study_area <- lengths(st_intersects(guide_sf, study_area)) > 0
  
  # Add this info back to the data.frame version of guide
  guide$in_study_area <- guide_sf$in_study_area
  
  #v=1
  for (v in 1) { #looping through Variables
    
    variable <- vars[v] # Getting the name of the variable
    print(variable)
    print(Sys.time())
    
    
    ## loading historical GCM
    nc_gcm <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/cmip6_data/subset/",model,"/historical/swe/snw_day_",model,"_",realization,"_historical_subset.nc"))
    
    ## loading historical bias_corrected GCM
    bc_gcm <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/cmip6_data/bias_correction/historical_swe_day_",model,"_BC.nc"))
    
    bc_gcm_array <- ncvar_get(bc_gcm, variable) # creating arrays of observed data
    
    gcm_array <- ncvar_get(nc_gcm, variable) # creating arrays of observed data
    
    gcm_lat <- ncvar_get(nc_gcm,"lat") #extracting latitudes from observed data
    gcm_lon <- ncvar_get(nc_gcm,"lon") #extracting longitudes from observed data
    
    res_lon_gcm <- gcm_lon[2]-gcm_lon[1]
    res_lat_gcm <- gcm_lat[2]-gcm_lat[1]
    
    
    ## loading downscaled data from 2010 to 2023
    nc_resample <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/resample/",model,"_resample.nc"))
    
    resample_array <- ncvar_get(nc_resample, variable) # creating arrays of predicted data
    resample_lat <- ncvar_get(nc_resample,"lat") #extracting latitudes from observed data
    resample_lon <- ncvar_get(nc_resample,"lon") #extracting longitudes from observed data
    
    #res <- 0.04166667 # resolution of both data sets
    
    res_lon_resample <- resample_lon[2]-resample_lon[1]
    res_lat_resample <- resample_lat[2]-resample_lat[1]
    
    
    #p=154
    registerDoParallel(3)
    
    
    data <- foreach(p = 1:length(guide$id), .combine=rbind, .multicombine = T,
                    .packages = c("doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                      
                      X <- guide[p,5] # lon for each pixel
                      Y <- guide[p,3] # lat for each pixel
                      in_out <- guide[p,4]
                      id <- guide[p,1]
                      
                      # Skip iteration if in_out == 0
                       if (in_out == 1 && guide[p, "in_study_area"]) {
                        # Calculate observed and predicted pixel positions
                        gcm_x <- abs(((gcm_lon[1] - X) / res_lon_gcm)) + 1.1# x pixel position in the observed data set
                        gcm_y <- as.double(length(gcm_lat) + ((Y - tail(gcm_lat, 1)) / res_lat_gcm)) # Y pixel position
                        
                        resample_x <- abs(((resample_lon[1] - X) / res_lon_resample)) + 1.1 # x pixel position in the pred data set
                        resample_y <- as.double(length(resample_lat) + ((Y - tail(resample_lat, 1)) / res_lat_resample))  # y pixel position
                        
                        # Extract observed and predicted data for the pixel
                        gcm_vec <- gcm_array[gcm_x, gcm_y, 274:12318]
                        gcm_vec[ gcm_vec < 0] <-0
                        
                        # Extract observed and predicted data for the pixel fo bc
                        bc_gcm_vec <- bc_gcm_array[gcm_x, gcm_y, 274:12318]
                        bc_gcm_vec[ bc_gcm_vec < 0] <-0
                        
                        
                        
                        
                        resample_vec <- resample_array[resample_x, resample_y, 1:12053]  # predicted data slice
                        # Assume your vector is:
                        daily_values <- resample_vec  # replace with actual variable
                        start_date <- as.Date("1981-10-01")
                        end_date <- as.Date("2014-09-30")
                        
                        # Generate full date sequence including leap days
                        dates <- seq(start_date, end_date, by = "day")
                        
                        # Remove Feb 29
                        non_leap_idx <- format(dates, "%m-%d") != "02-29"
                        resample_vec <- daily_values[non_leap_idx]
                        dates <- dates[format(dates, "%m-%d") != "02-29"]
                        
                       
                        
                        df1 <- data.frame(date = dates,year = format(dates,"%Y"),
                                          month = format(dates,"%m"),
                                          bc_gcm= bc_gcm_vec,gcm = gcm_vec, resample=resample_vec )
                        
                        df1$water_year <- ifelse(format(df1$date, "%m") >= "10",
                                                 as.integer(format(df1$date, "%Y")) + 1,
                                                 as.integer(format(df1$date, "%Y")))
                        
                        df1$water_month <- ifelse(df1$month %in% c("10", "11", "12"),
                                                  as.integer(df1$month) - 9,
                                                  as.integer(df1$month) + 3)
                        
                        
                        peak_r <- df1 %>%
                          group_by(water_year) %>%
                          summarise(peak = max(resample, na.rm = TRUE))
                        
                        peak_g <- df1 %>%
                          group_by(water_year) %>%
                          summarise(peak = max(gcm, na.rm = TRUE))
                        peak_b <- df1 %>%
                          group_by(water_year) %>%
                          summarise(peak = max(bc_gcm, na.rm = TRUE))
                        
                        
                        
                        peak_all <- data.frame(id=id, year = peak_r$water_year,resample=peak_r$peak, gcm=peak_g$peak,bc_gcm=peak_b$peak)
                        
                        return(peak_all)
                        
                        
                        gc()
                      }
                    }
    
    daily_avg <- data %>%
      group_by(year) %>%
      summarise(
        resample = mean(resample, na.rm = TRUE),
        gcm      = mean(gcm, na.rm = TRUE),
        bc_gcm   = mean(bc_gcm, na.rm = TRUE),
        .groups = "drop"
      )
 
    metrcis_average[[model]] <- daily_avg
  }
}



######################## save all results a table
map_list <- list()

for (i in 1:length(metrcis_average)) {
  layer <- metrcis_average[[i]]
  
  map <- ggplot(layer, aes(x = year)) +
    geom_line(aes(y = gcm, color = "GCM")) +
    geom_line(aes(y = bc_gcm, color = "Bias Corrected"))+
    geom_line(aes(y = resample, color = "Reference")) +
    scale_color_manual(values = c("GCM"="#AEC6CF",
                                  "Bias Corrected"="#4C9A9A" ,
                                  "Reference"= "#555555"))+
    labs(x="Year",y="SWE", color = "Data Source")+
    theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(size=8, vjust = -1.2,hjust=0.5),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10)
      
    )+scale_y_continuous(limits = c(0,250), breaks = seq(0,250,50))+
    scale_x_continuous(limits = c(1982,2014),breaks = c(seq(1982, 2014, 8), 2014)
    )+
    ggtitle(models[i,1])
    
  
  map_list[[i]] <- map
  
}
map_list[14]

legend <- get_legend(map_list[[1]])

map_list <- lapply(map_list, function(p) p + theme(legend.position = "none"))
maps <- grid.arrange(map_list[[1]],map_list[[2]],map_list[[3]],map_list[[4]],map_list[[5]],map_list[[6]],map_list[[7]],map_list[[8]],
                     map_list[[9]],map_list[[10]],map_list[[11]],map_list[[12]],map_list[[13]],map_list[[14]],legend,nrow=5,ncol=3)


ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/resample_bc_gcm.pdf",maps,dpi = 300,width = 18, height = 22,units = "cm")
ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/resample_bc_gcm.png",maps,dpi = 1300,width = 18, height = 22,units = "cm")
