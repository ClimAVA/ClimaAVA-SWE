
#loading libraries
library(ncdf4)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(Cairo)
library(ggspatial)

#loading shapefile of the required boders 
sw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/sw_map.shp")
nw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/nw_map.shp")
us_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/us_map.shp")
us_river <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/shps/rivers.shp")
west_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/west.shp")
st_crs(us_river) <- 4269  # Assign NAD83 (EPSG:4269)

#===============basins map chapter3

# Add a "basin" column to each shapefile before combining
basins <- c("colorado", "GSL", "sacremento", "willamette")
basins_list <- list()


for (b in basins) {
  shp <- paste0("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/shps/basins/", b, ".shp")
  basin_sf <- read_sf(shp)
  
  # Keep only geometry and basin name
  basin_clean <- st_sf(
    basin = b,
    geometry = st_geometry(basin_sf)
  )
  
  basins_list[[b]] <- basin_clean
}
all_basins <- do.call(rbind, basins_list)
all_basins[3,1] <- "Sacramento–San Joaquin"
# Define custom colors for each basin
basin_colors <- c(
  "colorado" = "#F49AC2",
  "GSL" = "#B2EBF2",
  "Sacramento–San Joaquin" = "#E6EE9C",
  "willamette" = "#FF6961"
)


# Final plot
case_study <- ggplot() +
  
  geom_sf(data = all_basins, aes(fill = basin), color = "black") +
  geom_sf(data = us_river, fill = NA, color = "gray", linewidth=0.03) +
  #geom_sf(data = sw_map, fill = NA, color = "black",linetype="dashed" ) + 
  #geom_sf(data = nw_map, fill = NA, color = "black",linetype="dashed") +
  geom_sf(data = st_geometry(nw_map), fill = NA, color = "black", linetype = "dashed", inherit.aes = FALSE) +
  geom_sf_text(data = nw_map, aes(label = NAME), inherit.aes = FALSE, size = 2.5, color =  "#505050", check_overlap = TRUE)+
  geom_sf(data = st_geometry(sw_map), fill = NA, color = "black", linetype = "dashed", inherit.aes = FALSE) +
  geom_sf_text(data = sw_map, aes(label = NAME), inherit.aes = FALSE, size = 2.5, color = "#505050", check_overlap = TRUE)+
  geom_sf(data = st_geometry(west_map), fill = NA, color = "black", linetype = "solid", linewidth = 0.3, inherit.aes = FALSE) +
  
  
  
  
  scale_fill_manual(values = basin_colors) +
  coord_sf(expand = T, clip = "off") +   # clip off to allow axis line visibility
  theme(
    plot.margin = unit(c(3, 0, 0, 0), "pt"),  # enough margin on left
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.line.x.bottom = element_line(color = "black", size = 0.3),
    axis.line.y.left = element_line(color = "black", size = 0.3),
    axis.ticks.length = unit(4, "pt"),
    axis.title = element_text(size = 10, family = "sans"),
    axis.text = element_text(size = 10, family = "sans"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.margin = margin(t = -2),
    legend.text = element_text(size = 12, family = "sans"),
    legend.spacing.x = unit(0, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.key = element_blank(),
    panel.grid = element_blank()
  ) +
  # Add north arrow
  annotation_north_arrow(
    location = "tr",  # Top-left corner
    which_north = "true",
    pad_x = unit(-0.35, "cm"),
    pad_y = unit(0.3, "cm"),
    style = north_arrow_minimal
  )+
  labs(x = "Longitude", y = "Latitude")

case_study

######################################################################################################################


library(ggplot2)
library(babynames) # provide the dataset: a dataframe called babynames
library(dplyr)
library(reshape2)
library(cowplot)
library(lemon)
library(lubridate)
library(ggpubr)
library(extrafont)
library(gridExtra)
library(tidyr)
library(grid)

basins <- c("colorado","GSL","sacremento","willamette")

models <- read.csv("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/climava_swe/excels/models_2025.csv")
datasets <- c("reference","downscale")


data <- data.frame(row_id=1:15340)

for (b in 1:4){
  
  basin <- basins[b]
  print(basin)
  
  for (d in 1:2){
    
    dataset <- datasets[d]
    print(dataset)
    
    if (d == 1) {
      
      file_path <- (paste0("/scratch/general/vast/u6055107/climava_swe/validation/basins_data/42_average/",basin,"_42_average.Rdata"))
      load(file_path)
      
      
      varname <- paste0(dataset,"_",basin)
      
      assign(varname,results_list)
      
      data[[varname]] <- results_list
      
      
    }
    if (d == 2) {
      
      for (m in c(1,12)){
        
        model <- models[m,1] # geeing the names of the model from the models table
        print(model)
        
        file_path <- (paste0("/scratch/general/vast/u6055107/climava_swe/validation/basins_data/42_average/",model,"_",basin,"_42_average.Rdata")) 
        
        load(file_path)
        
        
        varname <- paste0(model,"_",dataset,"_",basin)
        
        assign(varname,results_list)
        
        data[[varname]] <- results_list
        
        
      }
    }
    
  }
}


data <- data[,-1]
data$date <- seq(from=as.Date("1982/01/01"),to=as.Date("2023/12/31"),by="day")
data$year <- year(data$date)


colorado <- data[,c(13,1:3)]

colnames(colorado) <- c("date","Ref","FR","CR")
all_data_colorado <- pivot_longer(colorado, cols = -date, names_to = "variable", values_to = "value")
all_data_colorado$variable <- factor(all_data_colorado$variable, levels = c("Ref", "FR", "CR"))



GSL <- data[,c(13,4:6)]
colnames(GSL) <- c("date","Ref","FR","CR")
all_data_GSL <- pivot_longer(GSL, cols = -date, names_to = "variable", values_to = "value")
all_data_GSL$variable <- factor(all_data_GSL$variable, levels = c("Ref", "FR", "CR"))

sacremento <- data[,c(13,7:9)]
colnames(sacremento) <- c("date","Ref","FR","CR")
all_data_sacremento <- pivot_longer(sacremento, cols = -date, names_to = "variable", values_to = "value")
all_data_sacremento$variable <- factor(all_data_sacremento$variable, levels = c("Ref", "FR", "CR"))

willamette <- data[,c(13,10:12)]
colnames(willamette) <- c("date","Ref","FR","CR")
all_data_willamette <- pivot_longer(willamette, cols = -date, names_to = "variable", values_to = "value")
all_data_willamette$variable <- factor(all_data_willamette$variable, levels = c("Ref", "FR", "CR"))


four_basin <- list(all_data_colorado,all_data_GSL,all_data_sacremento,all_data_willamette)

names <- c("Colorado","GSL","Sacramento–San Joaquin","Willamette")

title_colors <- c( "#F49AC2",  "#B2EBF2", "#E6EE9C", "#FF6961")


maps <- list()



for (i in 1:4){
  
  basin <- four_basin[[i]]
  print(basin)
  
  max_y <- max(basin$value)
  min_y <- min(basin$value)
  
  
  map <- ggplot(basin,aes(x=date,y=value,colour = variable,linewidth = variable,linetype = variable))+
    geom_line()+
    scale_color_manual(values = c("#E69F00", "#56B4E9", "forestgreen"))+
    scale_linewidth_manual(values = c(0.6,0.4,0.2))+
    scale_linetype_manual(values = c("solid","solid","solid"))+ 
    scale_x_date(
      limits = as.Date(c("1982-01-01", "2023-12-31")),
      date_breaks = "5 years",
      date_labels = "%Y",
      expand = c(0, 0)
    )+ggtitle(names[i])+
    scale_y_continuous(
      limits = c(min_y, max_y),
      breaks = seq(floor(min_y), ceiling(max_y), by = 50)
    )+
    
    theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black",fill = NA,linewidth = 0.8),
          legend.position = "",legend.title = element_blank(),legend.text = element_text(size=9),
          axis.title.x = element_text(size = 9), axis.title.y = element_text(size=9),
          axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.52,vjust=-7,size = 10),#color =title_colors[i]),
                                    plot.margin = unit(c(0,5,0,0),"pt"), legend.spacing.x =unit(0.8,"cm"),legend.key.width = unit(1,"cm"))+
    labs(x=NULL,y=NULL)
    # Remove x-axis text/ticks for all but last plot
    if (i != 4) {
      map <- map +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else {
      map <- map +
        theme(axis.title.x = element_text(size = 9))
    }
  
  
  maps[[i]] <- map
  
}



maps[[4]]



# Combine the 4 plots vertically
combined_plots <- grid_arrange_shared_legend(
  maps[[1]], maps[[2]], maps[[3]], maps[[4]],
  position = "bottom", nrow = 4, ncol = 1,
  heights = c(1, 1, 1, 1)
)



final <- grid.arrange(
  arrangeGrob(
    textGrob("Daily SWE (mm)", rot = 90, vjust = 0.5, gp = gpar(fontsize = 10)),
    combined_plots,
    ncol = 2,
    widths = unit.c(unit(1, "lines"), unit(1, "npc") - unit(1, "lines"))
  )
)




# Add labels as grobs
label_a <- textGrob("(a)", x = unit(0.03, "npc"), y = unit(0.6, "npc"), just = c("left", "top"), gp = gpar(fontsize = 12, fontface = "bold"))
label_b <- textGrob("(b)", x = unit(0.03, "npc"), y = unit(0.95, "npc"), just = c("left", "top"), gp = gpar(fontsize = 12, fontface = "bold"))

# Combine label and plot with padding using arrangeGrob
case_study_labeled <- arrangeGrob(case_study, top = label_a)
final_labeled <- arrangeGrob(final, top = label_b)

# Now arrange the labeled plots
all <- grid.arrange(case_study_labeled, final_labeled, nrow = 2, heights = c(1, 1.8))

##################################################################################################################
#all <- grid.arrange(case_study,final,nrow=2, heights=c(1,1.8))
ggsave("/scratch/general/vast/u6055107/climava_swe/validation/basins_data/outputs/daily_swe.pdf",all,dpi = 300,units = "cm", width = 18, height = 23)
ggsave("/scratch/general/vast/u6055107/climava_swe/validation/basins_data/outputs/daily_swe.png",all,dpi = 1300,units = "cm", width = 18, height = 23)
######################################################################################################################################################
ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/daily_swe.pdf",all,dpi = 300,units = "cm", width = 18, height = 23)
ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/daily_swe.png",all,dpi = 1300,units = "cm", width = 18, height = 23)





