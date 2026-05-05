
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
title_colors <- c( "purple", "red", "orange", "blue")
##################################################################################### calculate max per year and the day of maximum value
colorado_max <- data[,c(14,13,1:3)]

colnames(colorado_max) <- c("year","date","Ref","FR","CR")
colorado_max <- pivot_longer(colorado_max, cols = - c(date,year), names_to = "variable", values_to = "value")

colorado_max <- colorado_max %>%
  mutate(year = year(date)) %>%              # Extract year from date column
  filter(year >= 1982 & year <= 2023) %>%
  group_by(variable, year) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  ungroup()
colorado_max$variable <- factor(colorado_max$variable, levels = c("Ref", "FR", "CR"))
#################################
GSL_max <- data[,c(14,13,4:6)]
colnames(GSL_max) <- c("year","date","Ref","FR","CR")
GSL_max <- pivot_longer(GSL_max, cols = - c(date,year), names_to = "variable", values_to = "value")

GSL_max <- GSL_max %>%
  mutate(year = year(date)) %>%              # Extract year from date column
  filter(year >= 1982 & year <= 2023) %>%
  group_by(variable, year) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  ungroup()
GSL_max$variable <- factor(GSL_max$variable, levels = c("Ref", "FR", "CR"))
#####################################

sacremento_max <- data[,c(14,13,7:9)]
colnames(sacremento_max) <- c("year","date","Ref","FR","CR")
sacremento_max <- pivot_longer(sacremento_max, cols = - c(date,year), names_to = "variable", values_to = "value")

sacremento_max <- sacremento_max %>%
  mutate(year = year(date)) %>%              # Extract year from date column
  filter(year >= 1982 & year <= 2023) %>%
  group_by(variable, year) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  ungroup()
sacremento_max$variable <- factor(sacremento_max$variable, levels = c("Ref", "FR", "CR"))
##########################

willamette_max <- data[,c(14,13,10:12)]
colnames(willamette_max) <- c("year","date","Ref","FR","CR")
willamette_max <- pivot_longer(willamette_max, cols = - c(date,year), names_to = "variable", values_to = "value")

willamette_max <- willamette_max %>%
  mutate(year = year(date)) %>%              # Extract year from date column
  filter(year >= 1982 & year <= 2023) %>%
  group_by(variable, year) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  ungroup()

willamette_max$variable <- factor(willamette_max$variable, levels = c("Ref", "FR", "CR"))
############################################################################

max_basin <- list(colorado_max,GSL_max,sacremento_max,willamette_max)



maxes <- list()

for (i in 1:4){
  
  basin <- max_basin[[i]]
  print(basin)
  
  max_y <- max(basin$value)
  min_y <- min(basin$value)
  
  map <- ggplot(basin,aes(x=date,y=value,colour = variable,linewidth = variable,linetype = variable))+
    geom_line()+
    scale_color_manual(values = c("#E69F00", "#56B4E9", "forestgreen"))+
    scale_linewidth_manual(values = c(0.4,0.4,0.4))+
    scale_linetype_manual(values = c("solid","solid","solid"))+
    scale_x_date(
      limits = as.Date(c("1982-01-01", "2023-12-31")),
      date_breaks = "5 years",
      date_labels = "%Y",
      expand = c(0, 0)
    )+ggtitle(names[i])+scale_y_continuous(
      limits = c(0, max_y),
      breaks = seq(floor(0), ceiling(max_y), by = 40)
    )+theme(panel.background = element_blank(),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.8),
            legend.position = "",legend.title = element_blank(),legend.text = element_text(size=8),
            axis.title.x = element_text(size = 8), axis.title.y = element_text(size=8),
            axis.text.x = element_text(size = 8),axis.text.y = element_text(size = 8),
            plot.title = element_text(hjust = 0.48,vjust=-8,size = 10),# color=title_colors[i]),
                                      plot.margin = unit(c(0,7,0,1),"pt"))+
    labs(x=NULL,y=NULL) 
  if (i != 4) {
    map <- map +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  } else {
    map <- map +
      theme(axis.title.x = element_text(size = 9))
  }
  
  
  maxes[[i]] <- map
  
}

maxes[[3]]


# Combine the 4 plots vertically
combined_plots <- grid_arrange_shared_legend(
  maxes[[1]], maxes[[2]], maxes[[3]], maxes[[4]],
  position = "bottom", nrow = 4, ncol = 1,
  heights = c(1, 1, 1, 1)
)



final_max <- grid.arrange(
  arrangeGrob(
    textGrob("Annual Maximum SWE (mm)", rot = 90, vjust = 0.5, gp = gpar(fontsize = 10)),
    combined_plots,
    ncol = 2,
    widths = unit.c(unit(1, "lines"), unit(1, "npc") - unit(1, "lines"))
  )
)


##################################################################################### calculate the day of maximum value
# Step 1: Select and rename relevant columns
colorado_max <- data[, c(14, 13, 1:3)]
colnames(colorado_max) <- c("year", "date", "Ref", "FR", "CR")

# Step 2: Add Day of Year
colorado_max$Day <- yday(colorado_max$date)

# Step 3: Convert to long format
colorado_max <- pivot_longer(colorado_max,
                             cols = c("Ref", "FR", "CR"),
                             names_to = "variable",
                             values_to = "value")

# Step 4: Get the max SWE value per year and variable, and extract the day of year
colorado_max <- colorado_max %>%
  filter(year >= 1982 & year <= 2023) %>%
  group_by(year, variable) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  dplyr::select(year, variable, date, Day, value) %>%
  ungroup()
colorado_max$variable <- factor(colorado_max$variable, levels = c("Ref", "FR", "CR"))
#################################
# Step 1: Select and rename relevant columns
GSL_max <- data[, c(14, 13, 4:6)]  # Columns for year, date, and GSL-related variables
colnames(GSL_max) <- c("year", "date", "Ref", "FR", "CR")

# Step 2: Add Day of Year
GSL_max$Day <- yday(GSL_max$date)

# Step 3: Convert to long format
GSL_max <- pivot_longer(GSL_max,
                        cols = c("Ref", "FR", "CR"),
                        names_to = "variable",
                        values_to = "value")

# Step 4: Get the max SWE value per year and variable, and extract the day of year
GSL_max <- GSL_max %>%
  filter(year >= 1982 & year <= 2023) %>%
  group_by(year, variable) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  dplyr::select(year, variable, date, Day, value) %>%
  ungroup()
GSL_max$variable <- factor(GSL_max$variable, levels = c("Ref", "FR", "CR"))
#####################################
# Step 1: Select and rename relevant columns
sacremento_max <- data[, c(14, 13, 7:9)]  # Columns for year, date, and Sacramento-related variables
colnames(sacremento_max) <- c("year", "date", "Ref", "FR", "CR")

# Step 2: Add Day of Year
sacremento_max$Day <- yday(sacremento_max$date)

# Step 3: Convert to long format
sacremento_max <- pivot_longer(sacremento_max,
                               cols = c("Ref", "FR", "CR"),
                               names_to = "variable",
                               values_to = "value")

# Step 4: Extract the day of year with the max SWE for each year and variable
sacremento_max <- sacremento_max %>%
  filter(year >= 1982 & year <= 2023) %>%
  group_by(year, variable) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  dplyr::select(year, variable, date, Day, value) %>%
  ungroup()

sacremento_max$variable <- factor(sacremento_max$variable, levels = c("Ref", "FR", "CR"))
##########################
# Step 1: Select and rename relevant columns
willamette_max <- data[, c(14, 13, 10:12)]  # Columns for year, date, and Willamette variables
colnames(willamette_max) <- c("year", "date", "Ref", "FR", "CR")

# Step 2: Add Day of Year
willamette_max$Day <- yday(willamette_max$date)

# Step 3: Convert to long format
willamette_max <- pivot_longer(willamette_max,
                               cols = c("Ref", "FR", "CR"),
                               names_to = "variable",
                               values_to = "value")



# Step 4: Extract the day of year with the max SWE for each year and variable
willamette_max <- willamette_max %>%
  filter(year >= 1982 & year <= 2023) %>%
  group_by(year, variable) %>%
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
  dplyr::select(year, variable, date, Day, value) %>%
  ungroup()


willamette_max$variable <- factor(willamette_max$variable, levels = c("Ref", "FR", "CR"))
############################################################################

max_basin <- list(colorado_max,GSL_max,sacremento_max,willamette_max)



days <- list()

for (i in 1:4){
  
  basin <- max_basin[[i]]
  print(basin)
  
  max_y <- 240
  min_y <- 80
  
  
  map <- ggplot(basin,aes(x=date,y=Day,colour = variable,linewidth = variable,linetype = variable))+
    geom_line()+
    scale_color_manual(values = c("#E69F00", "#56B4E9", "forestgreen"))+
    scale_linewidth_manual(values = c(0.4,0.4,0.4))+
    scale_linetype_manual(values = c("solid","solid","solid"))+
    scale_x_date(
      limits = as.Date(c("1982-01-01", "2023-12-31")),
      date_breaks = "5 years",
      date_labels = "%Y",
      expand = c(0, 0)
    )+ggtitle(names[i])+scale_y_continuous(
      limits = c(80, max_y),
      breaks = seq(floor(80), ceiling(max_y), by = 20)
    )+theme(panel.background = element_blank(),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.8),
            legend.position = "",legend.title = element_blank(),legend.text = element_text(size=8),
            axis.title.x = element_text(size = 8), axis.title.y = element_text(size=8),
            axis.text.x = element_text(size = 8),axis.text.y = element_text(size = 8),
            plot.title = element_text(hjust = 0.47,vjust=-8,size = 10),#color = title_colors[i]),
                                      plot.margin = unit(c(0,6,0,2),"pt"))+
    labs(x=NULL,y=NULL)
  
  if (i != 4) {
    map <- map +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  } else {
    map <- map +
      theme(axis.title.x = element_text(size = 9))
  }
  
  
  days[[i]] <- map
  
}

days[[3]]




# Combine the 4 plots vertically
combined_plots <- grid_arrange_shared_legend(
  days[[1]], days[[2]], days[[3]], days[[4]],
  position = "bottom", nrow = 4, ncol = 1,
  heights = c(1, 1, 1, 1)
)



final_days <- grid.arrange(
  arrangeGrob(
    textGrob("Day of Maximum SWE", rot = 90, vjust = 0.5, gp = gpar(fontsize = 10)),
    combined_plots,
    ncol = 2,
    widths = unit.c(unit(1, "lines"), unit(1, "npc") - unit(1, "lines"))
  )
)

# Add labels as grobs
label_a <- textGrob("(a)", x = unit(0.06, "npc"), y = unit(0.6, "npc"), just = c("left", "top"), gp = gpar(fontsize = 12, fontface = "bold"))
label_b <- textGrob("(b)", x = unit(0.06, "npc"), y = unit(0.6, "npc"), just = c("left", "top"), gp = gpar(fontsize = 12, fontface = "bold"))

# Combine label and plot with padding using arrangeGrob
final_max_l <- arrangeGrob(final_max, top = label_a)
final_days_l <- arrangeGrob(final_days, top = label_b)


two_plots <- grid.arrange(final_max_l,final_days_l,nrow=1,ncol=2)


ggsave("/scratch/general/vast/u6055107/climava_swe/validation/basins_data/outputs/max_day_42.pdf",two_plots,dpi = 300,units = "cm", width = 18, height = 18)
ggsave("/scratch/general/vast/u6055107/climava_swe/validation/basins_data/outputs/max_day_42.png",two_plots,dpi = 1300,units = "cm", width = 18, height = 18)



ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/max_day_42.pdf",two_plots,dpi = 300,units = "cm", width = 18, height = 18)
ggsave("/scratch/general/vast/u6055107/climava_swe/all_figs/max_day_42.png",two_plots,dpi = 1300,units = "cm", width = 18, height = 18)






