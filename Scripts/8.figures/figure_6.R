
#loading libraries
library(ncdf4)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(abind)
library(gridExtra)
library(cowplot)
library(grid)

#loading shapefile of the required boders 
sw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/sw_map.shp")
nw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/nw_map.shp")
us_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/us_map.shp")
west_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/west.shp")
models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")




#########################################################################################reference data
refernce_1 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_1982-1991.nc")
refernce_2 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_1992-2001.nc")
refernce_3 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_2002-2011.nc")
refernce_4 <- nc_open("/scratch/general/vast/u6055107/climava_swe/reference/stacked/swe_west_stack_2012-2023.nc")


lon <- ncvar_get(refernce_1,"lon")
lat <- ncvar_get(refernce_1,"lat")


var_r_1 <- ncvar_get(refernce_1,"swe")
var_r_2 <- ncvar_get(refernce_2,"swe")
var_r_3 <- ncvar_get(refernce_3,"swe")
var_r_4 <- ncvar_get(refernce_4,"swe")

var_r_4_t <- var_r_4[,,1:1096]


var_all <- abind::abind(var_r_1,var_r_2,var_r_3,var_r_4_t,along=3)


# Get time variable and convert to Date
time_1 <- ncvar_get(refernce_1,"time")
time_2 <- ncvar_get(refernce_2,"time")
time_3 <- ncvar_get(refernce_3,"time")
time_4 <- ncvar_get(refernce_4,"time")

# For input_1 (assumed starting 1982-01-01)
dates_1 <- as.Date("1982-01-01") + 0:(length(time_1) - 1)
# For input_2 (assumed starting 1998-01-01)
dates_2 <- as.Date("1992-01-01") + 0:(length(time_2) - 1)
dates_3 <- as.Date("2002-01-01") + 0:(length(time_3) - 1)
dates_4 <- as.Date("2012-01-01") + 0:(length(time_4) - 1)

# Combine
dates_all <- c(dates_1, dates_2,dates_3, dates_4)

years <- as.numeric(format(dates_all, "%Y"))

unique_years <- sort(unique(years))
unique_years <- unique_years[1:33]

yearly_max <- array(NA, dim = c(dim(var_all)[1], dim(var_all)[2], length(unique_years)))


for (i in seq_along(unique_years)) {
  idx <- which(years == unique_years[i])
  yearly_max[,,i] <- apply(var_all[,,idx], c(1, 2), max, na.rm = TRUE)
}

yearly_max[yearly_max == -Inf] <- NA

avg_of_max <- apply(yearly_max, c(1, 2), mean, na.rm = TRUE)



# --- Convert to data frame for ggplot ---
sw_data <- expand.grid(lon = lon, lat = lat) %>%
  mutate(avg_swe = as.vector(avg_of_max))

sw_data <- sw_data %>% filter(!is.na(sw_data$avg_swe))

sw_data_r <- rast(sw_data)


map_his <- ggplot() +
  geom_spatraster(data = sw_data_r, show.legend = T) +
  scale_fill_gradientn(
    name = "Reference Values(mm)",
    colors = c("#b68a5c", "#e3d6b4", "#b9d5c3", "#7fbfc1", 
               "#2b9cc0", "#4a8dd0", "#3b4fb6", "#1a1a5e"),
    values = scales::rescale(c(20, 80, 100, 400, 800, 1500, 2000, 3000)),
    limits = c(0, 3000),
    na.value = NA,
    #breaks = c(0, 40, 70, 90, 110, 150, 1000, 3000)
  ) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "pt"),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = -3, size = 7.5),
    legend.margin = margin(t = -20),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.4,"cm"),
    legend.title.position = "top",
    axis.text = element_blank(),
    axis.ticks = element_blank())+
  ggtitle("Reference (1982-2014)") +
  geom_sf(data = st_geometry(nw_map), fill = NA, color = "black", linetype = "dashed") +
  geom_sf(data = st_geometry(sw_map), fill = NA, color = "black", linetype = "dashed") +
  geom_sf(data = st_geometry(west_map), fill = NA, color = "black", linetype = "solid", linewidth=0.3)+ 
  coord_sf()

map_his
############################################################################# GCMs models
average_all_models <- list()
map_list <- list()
#== loading nc files
for (m in 1:13) {
  
  model <- models[m,1] # geeing the names of the model from the models table
  print(model)
  realization <- models[m,5]
  
  
  input_1 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",
                            model,"/historical/ClimAVA-SWE_west_",model,"_historical_",
                            realization,"_19810101-19971231.nc"))
  
  input_2 <- nc_open(paste0("/scratch/general/vast/u6055107/climava_swe/future/downscaling/",
                            model,"/historical/ClimAVA-SWE_west_",model,"_historical_",
                            realization,"_19980101-20141231.nc"))
  
  lon <- ncvar_get(input_1,"longitude")
  lat <- ncvar_get(input_1,"latitude")
  
  var_1 <- ncvar_get(input_1,"swe")  # [lon, lat, time]
  var_2 <- ncvar_get(input_2,"swe")
  var_all <- abind(var_1, var_2, along=3)
  
  # Get time variable and convert to Date
  time_1 <- ncvar_get(input_1,"time")
  time_2 <- ncvar_get(input_2,"time")
  
  
  # For input_1 (assumed starting 1981-01-01)
  dates_1 <- as.Date("1981-01-01") + 0:(length(time_1) - 1)
  
  # For input_2 (assumed starting 1998-01-01)
  dates_2 <- as.Date("1998-01-01") + 0:(length(time_2) - 1)
  
  # Combine
  dates_all <- c(dates_1, dates_2)
  
  years <- as.numeric(format(dates_all, "%Y"))
  
  unique_years <- sort(unique(years))
  
  unique_years <- unique_years[-1]
  
  yearly_max <- array(NA, dim = c(dim(var_all)[1], dim(var_all)[2], length(unique_years)))
  
  
  for (i in seq_along(unique_years)) {
    idx <- which(years == unique_years[i])
    yearly_max[,,i] <- apply(var_all[,,idx], c(1, 2), max, na.rm = TRUE)
  }
  
  yearly_max[yearly_max == -Inf] <- NA
  
  avg_of_max <- apply(yearly_max, c(1, 2), mean, na.rm = TRUE)
  
  # --- Convert to data frame for ggplot ---
  sw_data <- expand.grid(lon = lon, lat = lat) %>%
    mutate(avg_swe = as.vector(avg_of_max))
  
  
  sw_data <- sw_data %>% filter(!is.na(sw_data$avg_swe)) 
  
  sw_data <- rast(sw_data)
  
  diff <- sw_data - sw_data_r

  
  max_diff <- max(values(diff), na.rm = TRUE)
  min_diff <- min(values(diff), na.rm = TRUE)
  
  max_abs <- ceiling(max(abs(min_diff), abs(max_diff)) / 50) * 50
  limits <- c(-max_abs, max_abs)
  
  
  
  nc_close(input_1)
  nc_close(input_2)
  
  map <- ggplot() +
    geom_spatraster(data = diff, show.legend = T) +
    scale_fill_gradient2(low = "#2166ac",
                         mid = "#ffffff",
                         high = "#b2182b",
                         #trans = "sqrt",
                         midpoint = 0,
                         limits = c(-300,300),
                         name = ("Difference(mm)"), 
                         na.value = NA,
    ) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "pt"),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = -3, size = 6.4),
      legend.margin = margin(t = -20),
      legend.position = "bottom",
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(0.4,"cm"),
      legend.title.position = "top",
      #legend.title = element_text(size=8),
      #legend.text = element_text(size = 7),
      axis.text = element_blank(),
      axis.ticks = element_blank())+
    ggtitle(paste0("(",model,")  - (Reference)")) +
    geom_sf(data = st_geometry(nw_map), fill = NA, color = "black", linetype = "dashed") +
    geom_sf(data = st_geometry(sw_map), fill = NA, color = "black", linetype = "dashed") +
    geom_sf(data = st_geometry(west_map), fill = NA, color = "black", linetype = "solid", linewidth=0.3)+ 
    coord_sf()
  
  map_list[[m]] <- map
  average_all_models[[m]] <- sw_data
  
  gc()
}

map_list[1]

##############################################################################################################
# ===== AFTER LOOP: compute average across all models =====
# Combine all rasters into a single multi-layer raster
model_stack <- rast(average_all_models)

# Compute the mean raster (average SWE across models)
mean_of_all_models <- mean(model_stack, na.rm = TRUE)
diff_all_models <- mean_of_all_models - sw_data_r


average_all_map <- ggplot() +
  geom_spatraster(data = diff_all_models, show.legend = F) +
  scale_fill_gradient2(low = "#2166ac",
                       mid = "#ffffff",
                       high = "#b2182b",
                       #trans = "sqrt",
                       midpoint = 0,
                       limits = c(-300,300),
                       name = ("Difference(mm)"), 
                       na.value = NA,
  ) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "pt"),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = -3, size = 6.4),
    legend.margin = margin(t = -20),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.4,"cm"),
    legend.title.position = "top",
    #legend.title = element_text(size=8),
    #legend.text = element_text(size = 7),
    axis.text = element_blank(),
    axis.ticks = element_blank())+
  ggtitle("Ensemble mean difference") +
  geom_sf(data = st_geometry(nw_map), fill = NA, color = "black", linetype = "dashed") +
  geom_sf(data = st_geometry(sw_map), fill = NA, color = "black", linetype = "dashed") +
  geom_sf(data = st_geometry(west_map), fill = NA, color = "black", linetype = "solid", linewidth=0.3)+ 
  coord_sf()


saveRDS(average_all_models, "/scratch/general/vast/u6055107/climava_swe/all_figs/models_diff_average.rds")

#####################################################################################
#packages
library(sp)
library(ncdf4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(raster)
library(terra)
library(gridExtra)
library(lemon)
library(grid)


# Load variables names
vars <- c("swe")
# Load SSPs names
ssps <- c("historical","ssp245","ssp370","ssp585")

# creating year date from 1981 (first year) to 2100 (last year)
date <- data.frame("date" = (seq(from=as.Date("1982/01/01"),to= as.Date("2100/12/31"),by="year")))
# Extract just the year
date <- as.numeric(format(date$date, "%Y"))


basins <- c("colorado","GSL","sacremento","willamette")

### create en empty list to store the final orgrnized data for each basin

basin_mean <- list()
basin_seasonal <- list()

for (b in 1:4) {
  
  basin <- basins[b]
  print(basin)
  
  load(paste0("/scratch/general/vast/u6055107/climava_swe/basins_future/",basin,"_future.RData") )
  
  
  ##historical extraction############################################################
  hist <- data.frame(data_frames[1:13])
  #hist_mean_peak extraction
  hist_mean_peak <- hist[,seq(1,ncol(hist),by=2)]
  #mean, max,min
  hist_mean_peak$total_ave <- rowMeans(hist_mean_peak[1:13])
  hist_mean_peak$y_max <- apply(hist_mean_peak[,1:13],MARGIN = 1,max)
  hist_mean_peak$y_min <- apply(hist_mean_peak[,1:13],MARGIN = 1,min)
  hist_mean_peak <- hist_mean_peak[,14:16]
  
  #hist_seasonal extraction
  hist_seasonal <- hist[,seq(2,ncol(hist),by=2)]
  #mean, max,min
  hist_seasonal$total_ave <- rowMeans(hist_seasonal[1:13])
  hist_seasonal$y_max <- apply(hist_seasonal[,1:13],MARGIN = 1,max)
  hist_seasonal$y_min <- apply(hist_seasonal[,1:13],MARGIN = 1,min)
  hist_seasonal <- hist_seasonal[,14:16]
  
  ##ssp245 extraction########################################################################################
  ssp245 <- data.frame(data_frames[15:27])
  ssp245_mean_peak <- ssp245[,seq(1,ncol(ssp245),by=2)]
  
  #mean, max,min
  ssp245_mean_peak$total_ave <- rowMeans(ssp245_mean_peak[1:13])
  ssp245_mean_peak$y_max <- apply(ssp245_mean_peak[,1:13],MARGIN = 1,max)
  ssp245_mean_peak$y_min <- apply(ssp245_mean_peak[,1:13],MARGIN = 1,min)
  ssp245_mean_peak <- ssp245_mean_peak[,14:16]
  
  ssp245_seasonal <- ssp245[,seq(2,ncol(ssp245),by=2)]
  #mean, max,min
  ssp245_seasonal$total_ave <- rowMeans(ssp245_seasonal[1:13])
  ssp245_seasonal$y_max <- apply(ssp245_seasonal[,1:13],MARGIN = 1,max)
  ssp245_seasonal$y_min <- apply(ssp245_seasonal[,1:13],MARGIN = 1,min)
  ssp245_seasonal <- ssp245_seasonal[,14:16]
  ##ssp370 extraction########################################################################################
  ssp370 <- data.frame(data_frames[29:41])
  ssp370_mean_peak <- ssp370[,seq(1,ncol(ssp370),by=2)]
  
  #mean, max,min
  ssp370_mean_peak$total_ave <- rowMeans(ssp370_mean_peak[1:13])
  ssp370_mean_peak$y_max <- apply(ssp370_mean_peak[,1:13],MARGIN = 1,max)
  ssp370_mean_peak$y_min <- apply(ssp370_mean_peak[,1:13],MARGIN = 1,min)
  ssp370_mean_peak <- ssp370_mean_peak[,14:16]
  
  ssp370_seasonal <- ssp370[,seq(2,ncol(ssp370),by=2)]
  #mean, max,min
  ssp370_seasonal$total_ave <- rowMeans(ssp370_seasonal[1:13])
  ssp370_seasonal$y_max <- apply(ssp370_seasonal[,1:13],MARGIN = 1,max)
  ssp370_seasonal$y_min <- apply(ssp370_seasonal[,1:13],MARGIN = 1,min)
  ssp370_seasonal <- ssp370_seasonal[,14:16]
  ##ssp585 extraction########################################################################################
  ssp585 <- data.frame(data_frames[43:55])
  ssp585_mean_peak <- ssp585[,seq(1,ncol(ssp585),by=2)]
  
  #mean, max,min
  ssp585_mean_peak$total_ave <- rowMeans(ssp585_mean_peak[1:13])
  ssp585_mean_peak$y_max <- apply(ssp585_mean_peak[,1:13],MARGIN = 1,max)
  ssp585_mean_peak$y_min <- apply(ssp585_mean_peak[,1:13],MARGIN = 1,min)
  ssp585_mean_peak <- ssp585_mean_peak[,14:16]
  
  ssp585_seasonal <- ssp585[,seq(2,ncol(ssp585),by=2)]
  #mean, max,min
  ssp585_seasonal$total_ave <- rowMeans(ssp585_seasonal[1:13])
  ssp585_seasonal$y_max <- apply(ssp585_seasonal[,1:13],MARGIN = 1,max)
  ssp585_seasonal$y_min <- apply(ssp585_seasonal[,1:13],MARGIN = 1,min)
  ssp585_seasonal <- ssp585_seasonal[,14:16]
  
  ##reference extraction########################################################################################
  reference <- data.frame(data_frames[57])
  reference_mean_peak <- reference[1]
  colnames(reference_mean_peak)[1] <- "reference_mean"
  reference_seasonal <- reference[2]
  colnames(reference_seasonal)[1] <- "reference_seasonal"
  
  
  ################################# mean 
  mean_peak_all_model <- data.frame("year" =date,"reference_mean" = c(reference_mean_peak$reference_mean,rep(NA,77)),"hist_mean_average" = c(hist_mean_peak$total_ave,rep(NA,86)),"hist_mean_max" = c(hist_mean_peak$y_max,rep(NA,86)), "hist_mean_min" = c(hist_mean_peak$y_min,rep(NA,86)),
                                    "ssp245_mean_average" = c(rep(NA,33) ,ssp245_mean_peak$total_ave), "ssp245_mean_max" = c(rep(NA,33) ,ssp245_mean_peak$y_max), "ssp245_mean_min" = c(rep(NA,33) ,ssp245_mean_peak$y_min),
                                    "ssp370_mean_average" = c(rep(NA,33) ,ssp370_mean_peak$total_ave), "ssp370_mean_max" = c(rep(NA,33) ,ssp370_mean_peak$y_max), "ssp370_mean_min" = c(rep(NA,33) ,ssp370_mean_peak$y_min),
                                    "ssp585_mean_average" = c(rep(NA,33) ,ssp585_mean_peak$total_ave), "ssp585_mean_max" = c(rep(NA,33) ,ssp585_mean_peak$y_max), "ssp585_mean_min" = c(rep(NA,33) ,ssp585_mean_peak$y_min))
  
  
  
  ################################# seasonal 
  seasonal_all_model <- data.frame("year" = date,"reference_seasonal" = c(reference_seasonal$reference_seasonal,rep(NA,77)),"hist_seasonal_average" = c(hist_seasonal$total_ave,rep(NA,86)),"hist_seasonal_max" = c(hist_seasonal$y_max,rep(NA,86)), "hist_seasonal_min" = c(hist_seasonal$y_min,rep(NA,86)),
                                   "ssp245_seasonal_average" = c(rep(NA,33) ,ssp245_seasonal$total_ave), "ssp245_seasonal_max" = c(rep(NA,33) ,ssp245_seasonal$y_max), "ssp245_seasonal_min" = c(rep(NA,33) ,ssp245_seasonal$y_min),
                                   "ssp370_seasonal_average" = c(rep(NA,33) ,ssp370_seasonal$total_ave), "ssp370_seasonal_max" = c(rep(NA,33) ,ssp370_seasonal$y_max), "ssp370_seasonal_min" = c(rep(NA,33) ,ssp370_seasonal$y_min),
                                   "ssp585_seasonal_average" = c(rep(NA,33) ,ssp585_seasonal$total_ave), "ssp585_seasonal_max" = c(rep(NA,33) ,ssp585_seasonal$y_max), "ssp585_seasonal_min" = c(rep(NA,33) ,ssp585_seasonal$y_min))
  
  
  
  basin_mean[[basin]] <- mean_peak_all_model
  basin_seasonal[[basin]] <- seasonal_all_model
  
}



# Example plot names
names <- c("Colorado","GSL","Sacramento–San Joaquin","Willamette")


# to save all plots
plot_list <- list()

# loop over layers
for (i in 1:4) {
  layer <- basin_mean[[i]]
  
  t <- ggplot() +
    geom_line(data = layer, aes(x = date, y = reference_mean, color = "Reference"), na.rm = TRUE)+
    geom_line(data = layer, aes(x = date, y = hist_mean_average, color = "Historical"), na.rm = TRUE) +
    geom_line(data = layer, aes(x = date, y = ssp245_mean_average, color = "SSP245"), na.rm = TRUE) +
    geom_line(data = layer, aes(x = date, y = ssp370_mean_average, color = "SSP370"), na.rm = TRUE) +
    geom_line(data = layer, aes(x = date, y = ssp585_mean_average, color = "SSP585"), na.rm = TRUE) +
    scale_color_manual(
      values = c("Reference" = "black", 
                 "Historical" = "gray", 
                 "SSP245" = "green3", 
                 "SSP370" = "cadetblue", 
                 "SSP585" = "brown1"),
      breaks = c("Reference", "Historical", "SSP245", "SSP370", "SSP585")  # force order
    )+
    guides(color = guide_legend(title = NULL)) +
    
    geom_ribbon(data = layer, aes(x = date, ymin = hist_mean_min, ymax = hist_mean_max),
                fill = "gray", alpha = 0.5, na.rm = TRUE) +
    geom_ribbon(data = layer, aes(x = date, ymin = ssp245_mean_min, ymax = ssp245_mean_max),
                fill = "green3", alpha = 0.3, na.rm = TRUE) +
    geom_ribbon(data = layer, aes(x = date, ymin = ssp370_mean_min, ymax = ssp370_mean_max),
                fill = "cadetblue", alpha = 0.3, na.rm = TRUE) +
    geom_ribbon(data = layer, aes(x = date, ymin = ssp585_mean_min, ymax = ssp585_mean_max),
                fill = "brown1", alpha = 0.3, na.rm = TRUE) +
    
    labs(x = "Year", y = "SWE (mm)") +
    ggtitle(names[i]) +
    
    scale_y_continuous(limits = if (i == 1) c(0, 250) else if (i == 2) c(0, 350) else if (i == 3) c(0, 400) else c(0, 500)) +
    scale_x_continuous(limits = c(1980, 2100), breaks = seq(1980, 2100, by = 30)) +
    
    geom_hline(yintercept = mean(layer$hist_mean_average, na.rm = TRUE), linetype = "dashed") +
    
    theme(
      plot.title = element_text(hjust = 0.5,vjust = -5, size = 9),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(color = "black",size = 9),
      axis.text.y = element_text(color = "black",size = 9),
      axis.title = element_text(color = "black",size = 9),
      legend.position = c(0.20, 0.80),
      legend.key = element_blank(),
      legend.key.height = unit(0.4, "cm"),
      legend.text = element_text(size = 9),
      legend.background = element_rect(fill = "transparent"),
      plot.margin = margin(t = 0, r = 4, b = 0, l = 4)
    )
  
  # Save plot to list
  plot_list[[i]] <- t
}

# Display one of the plots
plot_list[[1]]


future_graph <- grid_arrange_shared_legend(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],position = "bottom",nrow = 2,ncol = 2)

###########################################################################################################################################

library(cowplot)
####################################################
#legend_hist <- get_legend(map_his)
get_legend <- function(myplot) {
  tmp <- ggplot_gtable(ggplot_build(myplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}

legend_hist <- get_legend(map_his)
grid.draw(legend_hist) # just to check
##################################
legend_model <- get_legend(map_list[[1]])
grid.draw(legend_model) # just to check
###############################################################################
map_his1 <- map_his + theme(legend.position = "none")
#map_list1 <- map_list
map_list <- lapply(map_list, function(p) p + theme(legend.position = "none"))


maps <- grid.arrange(map_his1, map_list[[1]],map_list[[2]],map_list[[3]],map_list[[4]],map_list[[5]],map_list[[6]],map_list[[7]],map_list[[8]],
                    map_list[[9]],map_list[[10]],map_list[[11]],map_list[[12]],map_list[[13]],average_all_map,nrow=3,ncol=5)

legends <- grid.arrange(legend_hist,legend_model, ncol=2)
separator <- linesGrob(y = unit(c(0, 0), "npc"), 
                       gp = gpar(col = "black", lwd = 1))
spacer <- grid.rect(gp = gpar(col = NA, fill = NA))

#######################label
label_a <- textGrob("(a)", x = unit(0.06, "npc"), y = unit(0.6, "npc"), just = c("left", "top"), gp = gpar(fontsize = 12, fontface = "bold"))
label_b <- textGrob("(b)", x = unit(0.06, "npc"), y = unit(0.6, "npc"), just = c("left", "top"), gp = gpar(fontsize = 12, fontface = "bold"))

# Combine label and plot with padding using arrangeGrob
future_graph <- arrangeGrob(future_graph, top = label_a)
maps <- arrangeGrob(maps, top = label_b)

#####################################################

all <- grid.arrange(future_graph,separator,maps,spacer,legends,nrow=5,ncol=1,heights=c(1,0.02,1.3,0.04,0.22))


ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/all_models_hist_diff.pdf",all,dpi = 300,width = 18, height = 22,units = "cm")
ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/all_models_hist_diff.png",all,dpi = 1500,width = 18, height = 22,units = "cm")
