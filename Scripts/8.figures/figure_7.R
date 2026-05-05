#packages ----
library(ncdf4)
library(doParallel)
library(foreign)
library(sp)
library(terra)
library(dplyr)
library(lubridate)

# Load models and grid data
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
# Define variables and their corresponding names
vars <- "swe"

# Load years
years <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/years.csv")
# Load SSPs names
ssps <- c("historical","ssp245","ssp370","ssp585","reference")


# Loop through models and variables
# m=1
# v=1
# s=2

res <- 0.04166667 
registerDoParallel(1)



# Create an empty list to store data frames
results_list <- list() 
data_frames <- list()

# Reading the guide file for the model, which contains geographic coordinates
guide <- read.dbf("/uufs/chpc.utah.edu/common/home/moraes-group2/swe/shp_guides/swe_west_guide.dbf")


for (v in 1) {  # Looping through variables
  variable <- vars[v]  # Get the name of the variable
  print(variable)
  
  for (s in 4:5){
    ssp <- ssps[s] # Get the name of the ssps from the ssp table
    print(ssp)
    
    
    
    ########################## adding reference data as observed 
    
    if(ssp == "reference"){
      
      # Load observed data 
      input_ref_1 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_1982-1991.nc")
      input_ref_2 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_1992-2001.nc")
      input_ref_3 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_2002-2011.nc")
      input_ref_4 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_2012-2023.nc")
      
      
      value_ref_1 <- ncvar_get(input_ref_1 , variable)
      value_ref_2 <- ncvar_get(input_ref_2 , variable)
      value_ref_3 <- ncvar_get(input_ref_3 , variable)
      value_ref_4 <- ncvar_get(input_ref_4 , variable)
      
      lon <- ncvar_get(input_ref_1 ,"lon") 
      lat <- ncvar_get(input_ref_1 , "lat")
      time <- ncvar_get(input_ref_1 , "time")
      nc_close(input_ref_1)
      nc_close(input_ref_2)
      nc_close(input_ref_3)
      nc_close(input_ref_4)
      
      #p=89429
      
      results_list <- foreach(p = 1:length(guide$lon), .combine=rbind, .multicombine = T,
                              .packages = c("doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                                
                                X <- guide[p,4] # lon for each pixel
                                Y <- guide[p,5] # lat for each pixel
                                in_out <- guide[p,3]
                                
                                
                                # Skip iteration if in_out == 0
                                if (in_out == 1) {
                                  # Calculate observed and predicted pixel positions
                                  ref_x <- abs(((lon[1] - X) / res)) + 1.1# x pixel position in the historical data set
                                  ref_y <- as.double(length(lat) + ((Y - tail(lat, 1)) / res))+0.1 # Y pixel position
                                  
                                  
                                  # Extract observed and predicted data for the pixel
                                  ref_vec_1 <- value_ref_1[ref_x, ref_y, 1:3652]
                                  ref_vec_2 <- value_ref_2[ref_x, ref_y, 1:3653]
                                  ref_vec_3 <- value_ref_3[ref_x, ref_y, 1:3652]
                                  ref_vec_4 <- value_ref_4[ref_x, ref_y, 1:1096]
                                  
                                  ref_vec <- c(ref_vec_1,ref_vec_2,ref_vec_3,ref_vec_4)
                                  
                                  ref_vec[ref_vec<0] <- 0
                                  
                                  # Number of full 365-day blocks
                                  n_years <- floor(length(ref_vec) / 365)
                                  
                                  # Get yearly maxima
                                  yearly_max <- sapply(1:n_years, function(i) {
                                    start <- (i - 1) * 365 + 1
                                    end <- i * 365
                                    max(ref_vec[start:end], na.rm = TRUE)
                                  })
                                  
                                  # Final average of yearly maxima
                                  average_per_pixel <- mean(yearly_max, na.rm = TRUE)
                                                         
                                  #average_per_pixel <- mean(ref_vec)
                                  average_per_pixel <- data.frame(x=X, y=Y, value=average_per_pixel)
                                  return(average_per_pixel)
                                  
                                }
                                
                                
                              }
     
      # Create a unique name for the data frame based on the variable and scenario
      df_name <- paste(variable, ssp, sep = "_")
      
      data_frames[[df_name]] <- results_list
      gc()
      
    }
    
    ########################################################################################
    
    if(ssp == "historical"){
      
      for (m in 1:13){  # Looping through models models 2 and 17
        model <- models[m, 1]  # Get the name of the model from the models table
        print(model)
        
        relization <- models[m,5]
        
        
        # Load observed data 
        input_hist_1 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/historical/ClimAVA-SWE_west_",model,"_historical_",relization,"_19810101-19971231.nc"))
        input_hist_2 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/historical/ClimAVA-SWE_west_",model,"_historical_",relization,"_19980101-20141231.nc"))
        
        value_hist_1 <- ncvar_get(input_hist_1 , variable)
        value_hist_2 <- ncvar_get(input_hist_2 , variable)
        
        lon <- ncvar_get(input_hist_1 ,"longitude") 
        lat <- ncvar_get(input_hist_1 , "latitude")
        time <- ncvar_get(input_hist_1 , "time")
        nc_close(input_hist_1)
        nc_close(input_hist_2)
        gc()
        #p=271
        
        results_list <- foreach(p = 1:length(guide$lon), .combine=rbind, .multicombine = T,
                                .packages = c("doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                                  
                                  X <- guide[p,4] # lon for each pixel
                                  Y <- guide[p,5] # lat for each pixel
                                  in_out <- guide[p,3]
                                  
                                  
                                  # Skip iteration if in_out == 0
                                  if (in_out == 1) {
                                    # Calculate observed and predicted pixel positions
                                    hist_x <- abs(((lon[1] - X) / res)) + 1.1# x pixel position in the historical data set
                                    hist_y <- as.double(length(lat) + ((Y - tail(lat, 1)) / res))+0.1 # Y pixel position
                                    
                                    
                                    # Extract observed and predicted data for the pixel
                                    hist_vec_1 <- value_hist_1[hist_x, hist_y, 274:6205]
                                    hist_vec_2 <- value_hist_2[hist_x, hist_y, 1:6205]
                                    
                                    hist_vec <- c(hist_vec_1,hist_vec_2)
                                    
                                    hist_vec[hist_vec<0] <- 0
                                    n_years <- floor(length(hist_vec) / 365)
                                    
                                    # Get yearly maxima
                                    yearly_max <- sapply(1:n_years, function(i) {
                                      start <- (i - 1) * 365 + 1
                                      end <- i * 365
                                      max(hist_vec[start:end], na.rm = TRUE)
                                    })
                                    
                                    # Final average of yearly maxima
                                    average_per_pixel <- mean(yearly_max, na.rm = TRUE)
                                    #average_per_pixel <- mean(hist_vec)
                                    average_per_pixel <- data.frame(x=X, y=Y, value=average_per_pixel)
                                    return(average_per_pixel)
                                    
                                  }
                                  
                                  
                                }
       
        # Create a unique name for the data frame based on the variable and scenario
        df_name <- paste(model,variable, ssp, sep = "_")
        
        data_frames[[df_name]] <- results_list
        gc()
      }
      
    
    } 
    ########################## Future data
    
    ########################################################################################
    
    
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585"){
      
      for (m in 1:13){  # Looping through models models 2 and 17
        model <- models[m, 1]  # Get the name of the model from the models table
        print(model)
        
        relization <- models[m,5]
        
        
        
        # Load observed data 
        input_fut_1 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/",ssp,"/ClimAVA-SWE_west_",model,"_",ssp,"_",relization,"_20150101-20291231.nc"))
        input_fut_2 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/",ssp,"/ClimAVA-SWE_west_",model,"_",ssp,"_",relization,"_20300101-20441231.nc"))
        input_fut_3 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/",ssp,"/ClimAVA-SWE_west_",model,"_",ssp,"_",relization,"_20450101-20591231.nc"))
        input_fut_4 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/",ssp,"/ClimAVA-SWE_west_",model,"_",ssp,"_",relization,"_20600101-20741231.nc"))
        input_fut_5 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/",ssp,"/ClimAVA-SWE_west_",model,"_",ssp,"_",relization,"_20750101-20891231.nc"))
        input_fut_6 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/",ssp,"/ClimAVA-SWE_west_",model,"_",ssp,"_",relization,"_20900101-21001231.nc"))
        
        #input_hist <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",model,"/historical/ClimAVA-SWE_west_",model,"_historical_",relization,"_19980101-20141231.nc"))
        
        
        value_fut_1 <- ncvar_get(input_fut_1 , variable)
        value_fut_2 <- ncvar_get(input_fut_2 , variable)
        value_fut_3 <- ncvar_get(input_fut_3 , variable)
        value_fut_4 <- ncvar_get(input_fut_4 , variable)
        value_fut_5 <- ncvar_get(input_fut_5 , variable)
        value_fut_6 <- ncvar_get(input_fut_6 , variable)
        
        
        lon <- ncvar_get(input_fut_1 ,"longitude") 
        lat <- ncvar_get(input_fut_1 , "latitude")
        time <- ncvar_get(input_fut_1 , "time")
        
        
        nc_close(input_fut_1)
        nc_close(input_fut_2)
        nc_close(input_fut_3)
        nc_close(input_fut_4)
        nc_close(input_fut_5)
        nc_close(input_fut_6)
        
        #p=88924
        future_chunks <- c(2015,2041,2071)
        
        gc()
        
        for (f in 1:3){
          
          future_year <- future_chunks[f]
          print(future_year)
            
          results_list <- foreach(p = 1:length(guide$lon), .combine=rbind, .multicombine = T,
                                  .packages = c("doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                                    
                                    X <- guide[p,4] # lon for each pixel
                                    Y <- guide[p,5] # lat for each pixel
                                    in_out <- guide[p,3]
                                    
                                    
                                    # Skip iteration if in_out == 0
                                    if (in_out == 1) {
                                      # Calculate observed and predicted pixel positions
                                      fut_x <- abs(((lon[1] - X) / res)) + 1.1# x pixel position in the historical data set
                                      fut_y <- as.double(length(lat) + ((Y - tail(lat, 1)) / res))+0.1 # Y pixel position
                                      
                                      if (future_year==2015){
                                      # Extract observed and predicted data for the pixel
                                      fut_vec_1 <- value_fut_1[fut_x, fut_y, 1:5475]
                                      fut_vec_2 <- value_fut_2[fut_x, fut_y, 1:4015]
                                      fut_vec <- c(fut_vec_1,fut_vec_2)
                                      }
                                      
                                      
                                      if (future_year ==2041){
                                      fut_vec_2 <- value_fut_2[fut_x, fut_y, 4016:5475]
                                      fut_vec_3 <- value_fut_3[fut_x, fut_y, 1:5475]
                                      fut_vec_4 <- value_fut_4[fut_x, fut_y, 1:4015]
                                      fut_vec <- c(fut_vec_2, fut_vec_3,fut_vec_4)
                                      }
                                      
                                      if (future_year ==2071){
                                      fut_vec_4 <- value_fut_4[fut_x, fut_y, 4016:5475]
                                      fut_vec_5 <- value_fut_5[fut_x, fut_y, 1:5475]
                                      fut_vec_6 <- value_fut_6[fut_x, fut_y, 1:4015]
                                      fut_vec <- c(fut_vec_4, fut_vec_5,fut_vec_6)
                                      }
                                      
                                      
                                      fut_vec[fut_vec<0] <- 0
                                      n_years <- floor(length(fut_vec) / 365)
                                      
                                      # Get yearly maxima
                                      yearly_max <- sapply(1:n_years, function(i) {
                                        start <- (i - 1) * 365 + 1
                                        end <- i * 365
                                        max(fut_vec[start:end], na.rm = TRUE)
                                      })
                                      
                                      # Final average of yearly maxima
                                      average_per_pixel <- mean(yearly_max, na.rm = TRUE)
                                      #average_per_pixel <- mean(fut_vec)
                                      average_per_pixel <- data.frame(x=X, y=Y, value=average_per_pixel)
                                      return(average_per_pixel)
                                      
                                      
                                    }
                                    
                                    
                                  }
       
          
          # Create a unique name for the data frame based on the variable and scenario
          df_name <- paste(model,variable, ssp,future_year, sep = "_")
          
          data_frames[[df_name]] <- results_list
          
          gc()
        
      }
      }
    }
  }
  

}


save(data_frames,file="/scratch/general/vast/u6055107/climava_swe/all_figs/future_data_whole_sw.RData")





##############################################################
library(ggplot2)
library(sf)
library(gridExtra)
library(cowplot)
library(grid)
library(lemon)
library(ggspatial)

load("/scratch/general/vast/u6055107/climava_swe/all_figs/future_data_whole_sw.RData")
sw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/sw_map.shp")
nw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/nw_map.shp")
us_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/us_map.shp")
west_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/west.shp")
# This extracts the third column from each data frame
third_columns <- lapply(data_frames, function(df) df[[3]])

# Combine all third columns into one data frame
result_df <- do.call(cbind, third_columns)
lon <- as.vector(data_frames[[1]][[1]])
lat <- as.vector(data_frames[[1]][[2]])



df_historical <- data.frame(lon = lon, lat = lat, value = result_df[, 40])

df_2015_2040 <- data.frame(lon = lon, lat = lat, value = rowMeans(result_df[, seq(1, 39, 3)]))

df_2041_2070 <- data.frame(lon = lon, lat = lat, value = rowMeans(result_df[, seq(2, 39, 3)]))

df_2071_2100 <- data.frame(lon = lon, lat = lat, value = rowMeans(result_df[, seq(3, 39, 3)]))

data_list <- list(df_historical, df_2015_2040,df_2041_2070,df_2071_2100)
name_list <-c("Reference (1982-2014)", "SSP585 (2015-2040)","SSP585 (2041-2070)","SSP585 (2071-2100)")



map_list<- list()

for (d in 1:length(data_list)) {
  
  layer <- data_list[[d]]
  
  map <- ggplot(layer, aes(x = lon, y = lat, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(
      name = "(mm)",
      colors = c(
        "#b68a5c", "#d2b788", "#e3d6b4", 
        "#cce4d4", "#b9d5c3", "#99d1cd", "#7fbfc1", 
        "#56b3c8", "#2b9cc0", "#4a8dd0", "#3b4fb6", "#1a1a5e"
      ),
      values = scales::rescale(c(0, 10, 30, 40, 100, 140, 180,
                                 220,400,600,1000,3000)),
      limits = c(0, 3000),
      breaks=c(0,500,1000,1500, 2000, 2500, 3000),
      #labels=c("0","500","1000",">1500"),
      na.value = NA,
      
    ) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "pt"),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = -3, size = 9),
      legend.margin = margin(t = 3),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title.position = "top",
      axis.text = element_text(size = 6.5),     # Show lat/lon numbers
      axis.ticks = element_line(color = "grey40"),  # Optional
      axis.line = element_line(color = "black")
      # axis.text = element_blank(),
      # axis.ticks = element_blank()
    ) +
    labs(x=NULL,y=NULL)+
    ggtitle(name_list[[d]]) +
    geom_sf(data = st_geometry(nw_map), fill = NA, color = "black", linetype = "dashed", inherit.aes = FALSE) +
    geom_sf_text(data = nw_map, aes(label = NAME), inherit.aes = FALSE, size = 2.5, color =  "#505050", check_overlap = TRUE)+
    geom_sf(data = st_geometry(sw_map), fill = NA, color = "black", linetype = "dashed", inherit.aes = FALSE) +
    geom_sf_text(data = sw_map, aes(label = NAME), inherit.aes = FALSE, size = 2.5, color = "#505050", check_overlap = TRUE)+
    geom_sf(data = st_geometry(west_map), fill = NA, color = "black", linetype = "solid", linewidth = 0.3, inherit.aes = FALSE) +
    
    # Add north arrow
    annotation_north_arrow(
      location = "tr",  # Top-left corner
      which_north = "true",
      pad_x = unit(-0.35, "cm"),
      pad_y = unit(0.3, "cm"),
      style = north_arrow_minimal
    )+
    
    coord_sf()
  
  map_list[[d]] <- map
}

map_list[[1]]

c <- grid_arrange_shared_legend(map_list[[1]],map_list[[2]],map_list[[3]],map_list[[4]],position = "bottom",nrow = 2,ncol = 2)




ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/future_ssp5.pdf",c,dpi = 300,width = 18, height = 18,units = "cm")
ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/future_ssp5.png",c,dpi = 1500,width = 18, height = 18,units = "cm")


############################################### calculation the area snow places
west_area <- 3100170

thresholds <- c(5, 25, 30, 50, 100, 400, 600, 1000)
cell_area_km2 <- 16

snow_area_by_threshold <- lapply(thresholds, function(th) {
  data.frame(
    Threshold = th,
    Historical = round((sum(df_historical$value >= th) * cell_area_km2) / west_area * 100, 2),
    Near_Future = round((sum(df_2015_2040$value >= th) * cell_area_km2) / west_area * 100, 2),
    Mid_Future = round((sum(df_2041_2070$value >= th) * cell_area_km2) / west_area * 100, 2),
    Far_Future = round((sum(df_2071_2100$value >= th) * cell_area_km2) / west_area * 100, 2)
  )
})

# Combine into a single data frame
snow_area_table <- do.call(rbind, snow_area_by_threshold)

