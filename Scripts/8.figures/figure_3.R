library(terra)
#library(mapproj)
library(viridisLite)
library(sf)
library(ggplot2)
library(lemon)
library(tidyterra)
library(gridExtra)
library(extrafont)
library(extrafontdb)
library(ggpubr)
library(tidyverse)
library(scales)
library(showtext)
library(cowplot)
library(patchwork)



#### EC-Earth3 model
load("/scratch/general/vast/u6055107/climava_swe/validation/metric_results/EC-Earth3_swe.Rdata")
EC_Earth3_swe<-data
s_EC <- data.frame(EC_Earth3_swe)
mean(s_EC$difference)
min(s_EC$difference)
max(s_EC$difference)
median(s_EC$difference)
sum(s_EC$difference < -75)
#### MPI-ESM1-2-LR model
load("/scratch/general/vast/u6055107/climava_swe/validation/metric_results/MPI-ESM1-2-LR_swe.Rdata")
MPI_ESM1_2_LR_swe<-data
s_MPI <- data.frame(MPI_ESM1_2_LR_swe)
mean(s_MPI$difference)
min(s_MPI$difference)
max(s_MPI$difference)
median(s_MPI$difference)


sw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/sw_map.shp")
nw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/nw_map.shp")
border <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/west.shp")


FR_Obs <- rast(EC_Earth3_swe[3])
CR_Obs <- rast(MPI_ESM1_2_LR_swe[3])
layers <- list(FR_Obs, CR_Obs)

title_name <- c(expression("FR-Ref [mean=-1.54]","CR-Ref [mean=-4.20]"))

pr_maps <- list()
for (i in 1:length(layers)) {
  layer <- layers[[i]]
  print(layer)
  

map <- ggplot()+ #setting dimensions
  geom_spatraster(data = layer,show.legend = T, # show.legend can be off
  )+
  scale_fill_gradient2("(mm)",low = "red3",mid='yellow', 
                       high = "dodgerblue3",midpoint=0,limits=c(-600,600),na.value = 'white',breaks=c(-600,-300,0,300,600))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10))+
  theme(legend.key.height = unit(0.5,"cm"),legend.key.width =unit(0.8,"cm"))+ 
  guides(fill=guide_colorbar(ticks.colour = NA,title.position = "top"))+
  theme(plot.margin = unit(c(-6,-5,-5,-5),"pt"))+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank())+
  ggtitle(title_name[i])+theme(plot.title = element_text(hjust = 0.5, vjust=-2, size=11))+
  theme(legend.position = "bottom",legend.justification = "center")+theme(legend.margin = margin(t=-20))+
  guides(fill = guide_colorbar(ticks = TRUE, ticks.colour = "black", title.position = "top"))+
  geom_sf(data = st_geometry(border), fill = NA, color = "black")+
  geom_sf(data = st_geometry(sw_map), fill = NA, color = "black", linetype="dashed")+
  geom_sf(data = st_geometry(nw_map), fill = NA, color = "black", linetype="dashed")+
  coord_sf()


pr_maps[[i]] <- map
}
FR_map <- pr_maps[[1]]
CR_map <- pr_maps[[2]]

min(s_EC$ob_max)
max(s_EC$ob_max)
median(s_EC$ob_max)

obs_graph <- ggplot()+ #setting dimensions
  geom_spatraster(data = rast(EC_Earth3_swe[1]),show.legend = T, # show.legend can be off
  )+
  scale_fill_gradientn(
    name = "(mm)",
    colors = c(
      "#b68a5c", "#d2b788", "#e3d6b4", 
      "#cce4d4", "#b9d5c3", "#99d1cd", "#7fbfc1", 
      "#56b3c8", "#2b9cc0", "#4a8dd0", "#3b4fb6", "#1a1a5e"
    ),
    values = scales::rescale(c(0, 10, 50, 70, 100, 140, 180,
                               220,400,600,1000,3000)),
    limits = c(0, 3000),
    breaks=c(0,1000,2000, 3000),
    labels=c("0","1000","2000","3000"),
    na.value = NA,
    
  ) + 
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10))+
  theme(legend.key.height = unit(0.5,"cm"),legend.key.width =unit(1.1,"cm"))+ 
  guides(fill=guide_colorbar(ticks.colour = NA,title.position = "top"))+
  theme(plot.margin = unit(c(-6,-5,-5,-5),"pt"))+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank())+
  ggtitle("Ref")+theme(plot.title = element_text(hjust = 0.5,vjust = -2,size= 11))+
  theme(plot.title.position="panel")+
  theme(legend.position = "bottom", legend.justification = "center")+theme(legend.margin = margin(t=-20))+
  geom_sf(data = st_geometry(border), fill = NA, color = "black")+
  geom_sf(data = st_geometry(sw_map), fill = NA, color = "black", linetype="dashed")+
  geom_sf(data = st_geometry(nw_map), fill = NA, color = "black", linetype="dashed")+
  guides(fill = guide_colorbar(ticks = TRUE, ticks.colour = "black", title.position = "top"))+

  coord_sf()
obs_graph
map_final<-grid.arrange(obs_graph,FR_map,CR_map,  nrow = 1,ncol=3)

colors()
ggsave("/scratch/general/vast/u6055107/climava_swe/validation/metric_results/prism_model_difference.pdf",map_final, dpi = 300,width = 18, height = 9,units = "cm")
ggsave("/scratch/general/vast/u6055107/climava_swe/validation/metric_results/prism_model_difference.png",map_final, dpi = 1500,width = 18, height = 9,units = "cm")






