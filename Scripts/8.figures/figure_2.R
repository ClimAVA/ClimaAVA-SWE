library(sf)
library(ggplot2)
library(terra)
library(ncdf4)
library(raster)
library(cowplot)
library(rlang)
library(patchwork)
library(gridExtra)
library(fields)
require(dplyr)
require(ggmap)
require(tidyr)
require(viridis)
require(maps)
#library(ncdf4.helpers)
library(tidyterra)
library(gridExtra)
library(lemon)
library(extrafont)
library(ggpubr)
library(tidyverse)
library(scales)
library(akima)
library(reshape2)
library(grid)
library(gridExtra)
library(magick)
library(png)

#### EC-Earth3 model
load("/scratch/general/vast/u1080428/metrics_EC-Earth3_1982_2023.RData")
fine <- data
fine_df <- data.frame(fine)
fine_df$x_bin <- cut(fine_df$mm_avg, breaks = c(0,20,60,100, 300, 3024),
                     labels = c("0–20","20–60","60–100", "100–300", "300-3024"),
                     include.lowest = TRUE)
sum(is.na(fine_df$x_bin))
fine_df <- fine_df[!is.na(fine_df$x_bin), ]
######################################
mean_R2_fine <- fine_df %>%
  group_by(x_bin) %>%
  summarize(mean_R2_fine = mean(R2, na.rm = TRUE))


r2_fine <- ggplot(fine_df, aes(x = x_bin, y = R2)) +
  geom_boxplot(outlier.shape=NA) +
  #geom_text(data = mean_R2_fine, aes(x = x_bin, y =0.5, label = round(mean_R2_fine, 2)),
            #color = "red", inherit.aes = FALSE, size=2.8) +
  labs(x = "SWE", y= NULL) +
  theme_bw()+
  scale_y_continuous(breaks=c(0, 1), limits=c(0,1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(axis.text.y=element_text(angle=90,hjust=0.6))+
  theme(axis.title.y = element_text(margin = unit(c(0, 2.5, 0, 0), "mm")))+
  theme(axis.text.x = element_text(angle=340,size = 8,colour = "black"))+
  theme(axis.text.y = element_text(size = 8,colour = "black"))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(plot.margin=margin(0,10,0,-5))+
  theme(aspect.ratio=0.65)


r2_fine
##############################################



mean_rmse_fine <- fine_df %>%
  group_by(x_bin) %>%
  summarize(mean_rmse_fine = mean(RMSE, na.rm = TRUE))



rmse_fine <- ggplot(fine_df, aes(x = x_bin, y = RMSE)) +
  geom_boxplot(outlier.shape=NA) +
  #geom_text(data = mean_rmse_fine, aes(x = x_bin, y = 90, label = round(mean_rmse_fine, 2)),
            #color = "red", inherit.aes = FALSE,vjust=1, size=2.8) +
  labs(x = "SWE", y = NULL) +
  theme_bw()+
  scale_y_continuous(limits = c(0, 160), breaks = c(0,160))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(axis.text.y=element_text(angle=90,hjust=0.6))+
  theme(axis.title.y = element_text(margin = unit(c(0, 2.5, 0, 0), "mm")))+
  theme(axis.text.x = element_text(angle=340,size = 8,colour = "black"))+
  theme(axis.text.y = element_text(size = 8,colour = "black"))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(plot.margin=margin(0,10,0,-5))+
  theme(aspect.ratio=0.65)
rmse_fine
#######################


mean_mpe_fine <- fine_df %>%
  group_by(x_bin) %>%
  summarize(mean_mpe_fine = mean(MPE, na.rm = TRUE))




mpe_fine <- ggplot(fine_df, aes(x = x_bin, y = MPE)) +
  geom_boxplot(outlier.shape=NA) +
  #geom_text(data = mean_mpe_fine, aes(x = x_bin, y = 0.95, label = round(mean_mpe_fine, 2)),
            #color = "red", inherit.aes = FALSE, vjust=1, size=2.8) +
  #ggtitle("MPE v. Average Swe")+
  labs(x = "SWE", y = NULL) +
  theme_bw()+
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1,1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(axis.text.y=element_text(angle=90,hjust=0.6))+
  theme(axis.title.y = element_text(margin = unit(c(0, 2.5, 0, 0), "mm")))+
  theme(axis.text.x = element_text(angle=340,size = 8,colour = "black"))+
  theme(axis.text.y = element_text(size = 8,colour = "black"))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(plot.margin=margin(0,10,0,-5))+
  theme(aspect.ratio=0.65)

mpe_fine
#############################################################

load("/scratch/general/vast/u1080428/metrics_MPI-ESM1-2-LR_1982_2023.RData")
coarse <- data
coarse_df <- data.frame(coarse)
coarse_df$x_bin <- cut(coarse_df$mm_avg, breaks = c(0,20,60,100, 300, 3024),
                     labels = c("0–20","20–60","60–100", "100–300", "300-3024"),
                     include.lowest = TRUE)
sum(is.na(coarse_df$x_bin))
coarse_df <- coarse_df[!is.na(coarse_df$x_bin), ]


mean_R2 <- coarse_df %>%
  group_by(x_bin) %>%
  summarize(mean_R2 = mean(R2, na.rm = TRUE))


mean_R2 <- mean_R2 %>%
  mutate(label_y = ifelse(x_bin == "300-3024", mean_R2 - 0.05, mean_R2))


r2_plot <- ggplot(coarse_df, aes(x = x_bin, y = R2)) +
  geom_boxplot(outlier.shape=NA) +
  #geom_text(data = mean_R2, aes(x = x_bin, y = 0.5, label = round(mean_R2, 2)),
            #color = "red", inherit.aes = FALSE, vjust=1, size=2.8) +
  #ggtitle("R2 v. Average Swe")+
  labs(x = "SWE", y = NULL) +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1), breaks = c(0,1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(axis.text.y=element_text(angle=90,hjust=0.6))+
  theme(axis.title.y = element_text(margin = unit(c(0, 2.5, 0, 0), "mm")))+
  theme(axis.text.x = element_text(angle=340,size = 8,colour = "black"))+
  theme(axis.text.y = element_text(size = 8,colour = "black"))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(plot.margin=margin(0,10,0,-5))+
  theme(aspect.ratio=0.65)


r2_plot
##############################################



mean_RMSE <- coarse_df %>%
  group_by(x_bin) %>%
  summarize(mean_rmse = mean(RMSE, na.rm = TRUE))


rmse_plot <- ggplot(coarse_df, aes(x = x_bin, y = RMSE)) +
  geom_boxplot(outlier.shape=NA) +
  #geom_text(data = mean_RMSE, aes(x = x_bin, y = 90, label = round(mean_rmse, 2)),
            #color = "red", inherit.aes = FALSE, vjust=1, size=2.8) +
  #ggtitle("RMSE v. Average Swe")+
  labs(x = "SWE", y = NULL) +
  theme_bw()+
  scale_y_continuous(limits = c(0, 160), breaks = c(0,160))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(axis.text.y=element_text(angle=90,hjust=0.6))+
  theme(axis.title.y = element_text(margin = unit(c(0, 2.5, 0, 0), "mm")))+
  theme(axis.text.x = element_text(angle=340,size = 8,colour = "black"))+
  theme(axis.text.y = element_text(size = 8,colour = "black"))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(plot.margin=margin(0,10,0,-5))+
  theme(aspect.ratio=0.65)
rmse_plot
#######################


mean_mpe <- coarse_df %>%
  group_by(x_bin) %>%
  summarize(mean_mpe = mean(MPE, na.rm = TRUE))


mpe_plot <- ggplot(coarse_df, aes(x = x_bin, y = MPE)) +
  geom_boxplot(outlier.shape=NA) +
  #geom_text(data = mean_mpe, aes(x = x_bin, y = 0.95, label = round(mean_mpe, 2)),
            #color = "red", inherit.aes = FALSE, vjust=1, size=2.8) +
  #ggtitle("MPE v. Average Swe")+
  labs(x = "SWE", y = NULL) +
  theme_bw()+
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1,1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  theme(axis.text.y=element_text(angle=90,hjust=0.6))+
  theme(axis.title.y = element_text(margin = unit(c(0, 2.5, 0, 0), "mm")))+
  theme(axis.text.x = element_text(angle=340,size = 8,colour = "black"))+
  theme(axis.text.y = element_text(size = 8,colour = "black"))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 8))+
  theme(plot.margin=margin(0,10,0,-5))+
  theme(aspect.ratio=0.65)

mpe_plot
### EC-Earth3 model




border <- st_read("/uufs/chpc.utah.edu/common/home/u6055107/Documents/codes/snow_validation_proposal/shp/west.shp")
state_border <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/west.shp")
sw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/sw_map.shp")
nw_map <- read_sf("/uufs/chpc.utah.edu/common/home/u6055107/Documents/shp/nw_map.shp")

models <- read.csv("/scratch/general/vast/u1080428/excel_files/models_2025.csv")
getwd()

for (m in c(1,13)){
  
  model <- models[m,1]
  print(model)
  
  metric_data <- load(paste0("/scratch/general/vast/u1080428/metrics_",model,"_1982_2023.RData"))
  
  #assign(model,data)
  
  if(m==1){ec <- data
  } else {
    mpi <- data
  }
  
  
}


variables <- mpi
r2<-rast(variables[1])
rmse<-rast(variables[2])
mpe<- rast(variables[3])



layers <- list(r2,rmse,mpe)

title_name <- c(expression("R"^2),"RMSE(mm)","MPE(mm)")



pr_maps <- list()
#i=2

for (i in 1:length(layers)) {
  layer <- layers[[i]]
  print(layer)
  
  if (i==1){
    breaks <- c(0,0.25,0.50,0.75,1)
  } else if (i==2){
    breaks <- c(0,40,80,120,160)
  } else {
    breaks <- c(-50,-25,0,25,50)
  }
  
  map <- ggplot()+ #setting dimensions
    geom_spatraster(data = layer,show.legend =T) +
    
    scale_fill_gradient2("", low=ifelse (i==1, "red3" , "dodgerblue3"), 
                         mid='yellow',high =ifelse (i==1, "dodgerblue3" , "red3"),
                         
                         midpoint=ifelse(i==1,0.50, ifelse(i==2,80,0)),limits=c(low=ifelse(i %in% c(1,2),0,-50),
                                                                                 high=ifelse(i==1,1, ifelse(i==2,160,50))),na.value = 'white',
                         breaks=breaks,
                         labels = scales::label_number(accuracy = ifelse(i==1,0.1,1)))+
    
    theme(legend.title = element_text(size=8),legend.text = element_text(size=8))+
    #theme(legend.key.height = unit(0.3,"cm"),legend.key.width =unit(1,"cm"))+
    theme(legend.position="bottom",legend.margin = margin(t=-10))+ guides(fill=guide_colorbar(ticks.colour = NA))+
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank()) +
    theme(plot.margin = margin(0, -10, 0, -10))+guides(fill = guide_colorbar(
      ticks.colour = NA,
      barheight = unit(0.3, "cm"),
      barwidth = unit(4, "cm")
    ))+
    #ggtitle(title_name[i])+ theme(plot.title = element_text(hjust =0.5,vjust = -1.4,size = 14))+
    geom_sf(data = st_geometry(state_border), fill = NA, color = "black")+
    geom_sf(data = st_geometry(sw_map), fill = NA, color = "black",linetype="dashed")+
    geom_sf(data = st_geometry(nw_map), fill = NA, color = "black",linetype="dashed")+
    
    coord_sf( )
  
  pr_maps[[i]] <- map
  
  
}


r2_map <- pr_maps[[1]]
rmse_map <- pr_maps[[2]]
mpe_map <- pr_maps[[3]]


vertical_label <- textGrob("CR", rot = 90, gp = gpar(fontsize = 14),hjust = -3)

r2_legend   <- get_legend(r2_map)
rmse_legend <- get_legend(rmse_map)
mpe_legend  <- get_legend(mpe_map)


r2_map_noleg   <- r2_map   + theme(legend.position = "none")
rmse_map_noleg <- rmse_map + theme(legend.position = "none")
mpe_map_noleg  <- mpe_map  + theme(legend.position = "none")



col1 <- vertical_label  # stays as is
col2 <- arrangeGrob(r2_map_noleg,r2_plot, ncol = 1, heights=c(1,0.75))
col3 <- arrangeGrob(rmse_map_noleg, rmse_plot, ncol = 1, heights=c(1,0.75))
col4 <- arrangeGrob(mpe_map_noleg, mpe_plot, ncol = 1, heights=c(1,0.75))

legend <- arrangeGrob(r2_legend, rmse_legend, mpe_legend, ncol=3)

col2_final <- arrangeGrob(col2,r2_legend,heights=c(1,0.15))
col3_final <- arrangeGrob(col3,rmse_legend,heights=c(1,0.15))
col4_final <- arrangeGrob(col4,mpe_legend,heights=c(1,0.15))
# Combine all columns into one layout
metrics_map_coarse <- grid.arrange(col1, col2_final, col3_final, col4_final,
                                 ncol = 4,
                                 widths = c(0.1, 1, 1, 1))



metrics_map <- grid.arrange(metrics_map_fine,metrics_map_coarse,ncol=1)

ggsave("/scratch/general/vast/u6055107/climava_swe/validation/metric_results/final_metrics.pdf",metrics_map,height=23.5,width = 18,dpi=300, units = "cm")
ggsave("/scratch/general/vast/u6055107/climava_swe/validation/metric_results/final_metrics.png",metrics_map,height=23.5,width = 18,dpi=1500, units = "cm")




































