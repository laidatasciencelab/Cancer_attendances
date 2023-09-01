# Figures

The objective of this code is to generate figures

R version 3.6.2

## Importing packages
```R
library(tidyverse)
library(data.table)
library(dplyr)
library(pals)
library(ggsci)
library(viridis)
library(ggallin)
```

## Input files
1) Data files for figures

## Figure 1A - 1B
```R
# Generated using external software
```

## Figure 2
```R
# Cancer diagnosis weekly rate change against 5 year average

# Load data 
add_data = fread("Fig2_addenum.csv")

add_data_plot = ggplot(add_data, aes(x = week, y = pc_change, group = year, color = as.factor(year))) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0, size = 1, color = "#000000") +

  scale_x_continuous(expand = c(0, 0), limits = c(0, 52), n.breaks=52) +
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 10)) +
  scale_color_manual(values = c("#000000", "#009E73", "#E69F00", "#56B4E9")) +
  
  ggtitle("Percentage change of weekly incident cancer diagnosis rates against weekly 5 year average rates (2018-2021)") +
  xlab("\nTime (week)") +
  ylab("\nPercentage change") +
  labs(color= "Year") +
  
  theme_bw() +
  theme(
    plot.title = element_text(color="black",size=14,face="bold",hjust=0.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

add_data_plot
```

## Figure 3A
```R
# Volume ratio pc changes by stratified group 

# Load data 
hm_data2 = fread("Fig2B_volume.csv")

hm_data2$Stratification = factor(hm_data2$Stratification, levels = unique(hm_data2$Stratification))

# Data splits for heatmap threshold (options)
data_split_colors_viridis = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#3fbc73", "#5ec962", "#addc30", "#fde725")

## make balanced data splits
mat_breaks <- quantile_breaks(hm_data2$Pc_change, n = 12) # this generates 12 breaks/datapoints
mat_breaks

hm_data2$discrete = cut(hm_data2$Pc_change, breaks = c(-84.530000, -71.888182, -66.629091, -49.577273, -47.443636, -36.960000, -32.730000, -14.620909,1.710909, 4.866364, 9.726364, 208.000000))

# Facet ordering 
hm_data2$Stratification_group_o = factor(hm_data2$Stratification_group, levels = c("Age", "Comorbidities", "Ethnicity", "IMD", "Diagnosis time", "Primary cancer", "Secondary cancer", "Region"))
hm_data2$Time_period_o = factor(hm_data$Time_period, levels = c("PP-LD1", "LD1-MR", "MR-LD2", "LD2-LD3", "LD3-LL"))
#hm_data2$Time_period_o = factor(hm_data2$Time_period, levels = c("Pre-pandemic - Lockdown 1", "Lockdown 1 - Minimal restrictions", "Minimal restrictions - Lockdown 2", "Lockdown 2 - Lockdown 3", "Lockdown 3 - Leaving lockdown"))

data_split_colors_viridis = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#addc30", "#fde725")

# Plotting
hm_plot2 = ggplot(hm_data2, aes(x = Time_period, y = reorder(Stratification, Ordering))) +
  geom_tile(aes(fill = discrete)) +
  scale_fill_manual(breaks = levels(hm_data2$discrete),
                    values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE) +
  facet_grid(Stratification_group_o ~ Time_period_o, scales = "free", space = "free") +

  geom_text(aes(color="white", label = sprintf("%0.2f", round(Pc_change, digits = 2))), show.legend = FALSE) +
  scale_colour_manual(values=c("#FFFFFF")) +

  theme_bw() +
  theme(#axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, family = "Helvetica"),
    axis.text.y = element_text(size=12),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    strip.text.y.right = element_text(angle = 0),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.background =element_rect(fill="grey80"),
    legend.position = "right") +

  #labs(x = "", y = "") +
  labs(fill = "Threshold") +

  ggtitle("Attendance volume ratios between time periods (%)") +
  xlab("\nTime period") +
  ylab("")

hm_plot2
```

## Figure 4A
```R
# Rate ratio pc changes by stratified group 

# Load data 
hm_data = fread("Fig2A_rate.csv")

# Preprocessing
hm_data$Stratification = factor(hm_data$Stratification, levels = unique(hm_data$Stratification))

# Data splits for heatmap threshold (options)
data_split_colors_viridis = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725")

# Option for quantile breaks
quantile_breaks <- function(xs, n = 12) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n), na.rm=TRUE)
  breaks[!duplicated(breaks)]
}

## make balanced data splits
mat_breaks <- quantile_breaks(hm_data$Pc_change, n = 12) # this generates 12 breaks/datapoints
mat_breaks

hm_data$discrete = cut(hm_data$Pc_change, breaks = c(-80.850000, -34.487273, -30.853636, -11.971818,  -5.013636,  -1.380909,   3.237273,  12.970909,  34.236364, 233.120909, 421.590909, 541.320000))

# Facet ordering 
hm_data$Stratification_group_o = factor(hm_data$Stratification_group, levels = c("Age", "Comorbidities", "Ethnicity", "IMD", "Diagnosis time", "Primary cancer", "Secondary cancer", "Region"))
hm_data$Time_period_o = factor(hm_data$Time_period, levels = c("PP-LD1", "LD1-MR", "MR-LD2", "LD2-LD3", "LD3-LL"))
#hm_data$Time_period_o = factor(hm_data$Time_period, levels = c("Pre-pandemic - Lockdown 1", "Lockdown 1 - Minimal restrictions", "Minimal restrictions - Lockdown 2", "Lockdown 2 - Lockdown 3", "Lockdown 3 - Leaving lockdown"))

# Plotting
hm_plot = ggplot(hm_data, aes(x = Time_period, y = reorder(Stratification, Ordering))) +
  geom_tile(aes(fill = discrete)) +
  scale_fill_manual(breaks = levels(hm_data$discrete),
                    values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE) +
  facet_grid(Stratification_group_o ~ Time_period_o, scales = "free", space = "free") +

  geom_text(aes(color="white", label = sprintf("%0.2f", round(Pc_change, digits = 2))), show.legend = FALSE) +
  scale_colour_manual(values=c("#FFFFFF")) +

  theme_bw() +
  theme(#axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, family = "Helvetica"),
        axis.text.y = element_text(size=12),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.y.right = element_text(angle = 0),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="grey80"),
        legend.position = "right") +

  #labs(x = "", y = "") +
  labs(fill = "Threshold") +

  ggtitle("Attendance rate ratio between time periods (%)") +
  xlab("\nTime period") +
  ylab("")

hm_plot
```

## Figure 3B/4B
```R
# Region plot for rate ratios

# Load data 
region_plot = fread("Fig2C_rate_region.csv")
region_plot$discrete = cut(region_plot$value, breaks = c(-80.840000, -34.487273, -30.853636, -11.971818,  -5.013636,  -1.380909,   3.237273,  12.970909,  34.236364, 233.120909, 421.590909, 541.320000))

# Load coordinate map
prac_region_map = fread("S02.prac_region_map_data.sparse_60_small.2021-05-04.csv")

# Load source function
plot_map_English_prac_regions = function(data, map = prac_region_map){
  
  # Objective: create UK map with ggplot of English CPRD prac regions
  # Input: data: data.frame with two columns 1) 'value' = metric used for color filling
  #        and 2) column 'prac_region' with integer value for prac region in range 
  #        from 1:10
  #        map: loaded dataset prac_region_map containing the UK shape data 
  #        annotated with CPRD prac regions for England
  # Output: ggplot object which can be further individualized with color scale 
  #         and title
  
  require(tidyverse)
  
  # first combine shape data with input values
  plot_data = map %>% 
    left_join(data)
  
  # plot UK map
  out = plot_data %>% 
    ggplot(aes(x= long,
               y= lat,
               group = group,
               fill= discrete),
           color=NA) + 
    geom_polygon() +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'bottom',
          axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(fill = "Threshold") +
    #set aspect ratio for plot
    coord_fixed(1) 
  
  return(out)
}
# Plotting
# LD1_PP
rg_v1 = region_plot %>% filter(time_period == "PP-LD1") %>% select(c("prac_region", "discrete"))

rg_v1_plot = plot_map_English_prac_regions(data = (rg_v1),
                                           map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg_v1_plot

# MR_LD1
rg_v2 = region_plot %>% filter(time_period == "LD1-MR") %>% select(c("prac_region", "discrete"))

rg_v2_plot = plot_map_English_prac_regions(data = (rg_v2),
                                           map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg_v2_plot

# LD2_MR
rg_v3 = region_plot %>% filter(time_period == "MR-LD2") %>% select(c("prac_region", "discrete"))

rg_v3_plot = plot_map_English_prac_regions(data = (rg_v3),
                                           map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg_v3_plot

# LD3_LD2
rg_v4 = region_plot %>% filter(time_period == "LD2-LD3") %>% select(c("prac_region", "discrete"))

rg_v4_plot = plot_map_English_prac_regions(data = (rg_v4),
                                           map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg_v4_plot

# LL_LD3
rg_v5 = region_plot %>% filter(time_period == "LD3-LL") %>% select(c("prac_region", "discrete"))

rg_v5_plot = plot_map_English_prac_regions(data = (rg_v5),
                                           map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg_v5_plot

# Region plot for volume ratios

# Load data
region_plot2 = fread("Fig2D_volume_region.csv")
region_plot2$discrete = cut(region_plot2$value, breaks = c(-84.530000, -71.888182, -66.629091, -49.577273, -47.443636, -36.960000, -32.730000, -14.620909,1.710909, 4.866364, 9.726364, 208.000000))

# Plotting
# LD1_PP
rg2_v1 = region_plot2 %>% filter(time_period == "PP-LD1") %>% select(c("prac_region", "discrete"))

rg2_v1_plot = plot_map_English_prac_regions(data = (rg2_v1),
                                            map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg2_v1_plot

# MR_LD1
rg2_v2 = region_plot2 %>% filter(time_period == "LD1-MR") %>% select(c("prac_region", "discrete"))

rg2_v2_plot = plot_map_English_prac_regions(data = (rg2_v2),
                                            map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg2_v2_plot

# LD2_MR
rg2_v3 = region_plot2 %>% filter(time_period == "MR-LD2") %>% select(c("prac_region", "discrete"))

rg2_v3_plot = plot_map_English_prac_regions(data = (rg2_v3),
                                            map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg2_v3_plot

# LD3_LD2
rg2_v4 = region_plot2 %>% filter(time_period == "LD2-LD3") %>% select(c("prac_region", "discrete"))

rg2_v4_plot = plot_map_English_prac_regions(data = (rg2_v4),
                                            map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg2_v4_plot

# LL_LD3
rg2_v5 = region_plot2 %>% filter(time_period == "LD3-LL") %>% select(c("prac_region", "discrete"))

rg2_v5_plot = plot_map_English_prac_regions(data = (rg2_v5),
                                            map = prac_region_map) +
  scale_fill_manual(values = c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c", "#28ae80", "#3fbc73", "#5ec962", "#84d44b", "#addc30", "#fde725"),
                    drop = FALSE)

rg2_v5_plot
```

## Figure 5
```R
# Lollipop plot for intra-rate comparison

# Load data
lolli_data = fread("Fig3_intrarate.csv")

# Facet ordering 
lolli_data$Stratification_group_o = factor(lolli_data$Stratification_group, levels = c("Age", "Comorbidities", "Ethnicity", "IMD", "Diagnosis", "Primary cancer", "Secondary cancer", "Region"))
lolli_data$Time_period_o = factor(lolli_data$Time_period, levels = c("Pre-pandemic", "Lockdown 1", "Minimal restrictions", "Lockdown 2", "Lockdown 3", "Leaving lockdown"))

# Plotting
lolli_plot = ggplot(lolli_data, aes(y = Pc_change, x = reorder(Stratification, Ordering))) +
  geom_hline(yintercept = 0, 
             color = "grey65", size = 2) +
  # segmenting is the lollipop line
  geom_segment(aes(x=reorder(Stratification, Ordering), 
                   xend=reorder(Stratification, Ordering), 
                   y=0, 
                   yend=Pc_change),size=1,color="grey65") +
  geom_point(size=7, alpha=1, aes(color=as.factor(Stratification_group))) +
  geom_text(aes(label = sprintf("%0.1f", round(Pc_change, digits = 1))), color="white", size =2) +
  
  facet_grid(~Time_period_o, scales = "fixed", space = "free") +
  #facet_wrap(~Time_period_o,nrow=1, scales="fixed") +
  coord_flip() +
  
  scale_y_continuous(trans = "pseudo_log", breaks = c(-100, 0, 100, 600)) +
  
  scale_color_manual(values=c("#12096e","#0a81ff","#c35483","#13c53f","#ff00b6","#1f9698","#ffd300","#9d25f8")) +
  
  theme_bw() +
  theme(panel.grid.major = element_line(),
        panel.grid.major.x = element_blank() ,
        #panel.grid.major.y = element_line( size=.1, color="black" ),
        panel.grid.minor = element_blank(),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=13),
        legend.position = "right") +
  
  guides(col= guide_legend(title= "Groups")) +
  ggtitle("Attendance rate ratio (% change, intragroup)") +
  xlab("") + ylab("") 
  
lolli_plot
```

## Figure 6
```R
# Seasonality GP 

# Load data
season_gp = fread("Fig4A_GP.csv")

season_gp_plot = ggplot(season_gp, aes(x = time, y = count, group = year, color = as.factor(year))) +
  geom_vline(xintercept = 13, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 26, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 39, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 1, size = 1, color = "#56B4E9") +
  geom_vline(xintercept = 12, size = 1, color = "#56B4E9") +
  geom_line(size = 2) +
  
  
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52), n.breaks=52) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15000)) +
  scale_color_manual(values = c("#000000", "#009E73", "#E69F00", "#56B4E9")) +
  
  ggtitle("GP attendance (2010-2021)") +
  xlab("\nTime (year-week)") +
  ylab("\nAttendance count") +
  labs(color= "Year") +
  
  theme_bw() +
  theme(
    plot.title = element_text(color="black",size=14,face="bold",hjust=0.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

season_gp_plot

# Seasonality APC 

# Load data
season_APC = fread("Fig4B_HESAPC.csv")

season_APC_plot = ggplot(season_APC, aes(x = time, y = count, group = year, color = as.factor(year))) +
  geom_vline(xintercept = 13, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 26, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 39, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 1, size = 1, color = "#56B4E9") +
  geom_vline(xintercept = 12, size = 1, color = "#56B4E9") +
  geom_line(size = 2) +
  
  
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52), n.breaks=52) +
  scale_y_continuous(expand = c(0, 0), limits = c(2500, 7500)) +
  scale_color_manual(values = c("#000000", "#009E73", "#E69F00", "#56B4E9")) +
  
  ggtitle("HES APC attendance (2010-2021)") +
  xlab("\nTime (year-week)") +
  ylab("\nAttendance count") +
  labs(color= "Year") +
  
  theme_bw() +
  theme(
    plot.title = element_text(color="black",size=14,face="bold",hjust=0.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

season_APC_plot

# Seasonality OP

# Load data
season_op = fread("Fig4C_HESOP.csv")

season_op_plot = ggplot(season_op, aes(x = time, y = count, group = year, color = as.factor(year))) +
  geom_vline(xintercept = 13, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 26, size = 1, color = "#E69F00") +
  geom_vline(xintercept = 39, size = 1, color = "#E69F00") +
  geom_line(size = 2) +

  
  scale_x_continuous(expand = c(0, 0), limits = c(0, 52), n.breaks=52) +
  scale_y_continuous(expand = c(0, 0), limits = c(12500, 120000)) +
  scale_color_manual(values = c("#000000", "#009E73", "#E69F00")) +
  
  ggtitle("HES OP attendance (2010-2020)") +
  xlab("\nTime (year-week)") +
  ylab("\nAttendance count") +
  labs(color= "Year") +
  
  theme_bw() +
  theme(
    plot.title = element_text(color="black",size=14,face="bold",hjust=0.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

season_op_plot
```

## Figure 7
```R
# ARIMA modelling

# Load data
ITS_model = fread("Fig5_ARIMA.csv")

ITS_plot = ggplot(ITS_model, aes(time_from_base, count, group=Group,fill=Group, color=Group)) +
  geom_line(size=2)+
  geom_point(aes(time_from_base, Original), colour = "black", size=1) +
  geom_ribbon(aes(ymin=ITS_model$lower_ci, ymax=ITS_model$upper_ci), show.legend = FALSE, alpha=0.1, colour = NA) + 
  geom_vline(xintercept = 117, size = 1, color = "#000000", linetype = 2) +
  
  scale_colour_manual(values=c("#E69F00","#0072B2")) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  
  ggtitle("\nInterrupted Time Series Model\n") +
  xlab("\n2018 - 2021 (in weeks)\n") +
  ylab("\nAttendance count\n") +
  #scale_x_continuous(limits= c(0,180), breaks=seq(0,180, by=4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 180), breaks=seq(0,180, by=4)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 150000)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 150000)) +
  #ylim(0, 150000) +
  
  theme_bw() +
  theme(
    plot.title = element_text(color="black",size=14,face="bold",hjust=0.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.title=element_blank())

ITS_plot
```

## Figure 8
```R
# Correlation plot of attendance and mortality inverse pc changes 

# Load data
corr_morr = fread("Fig6_corr.csv")

corr_mor_plot = ggplot(corr_morr) +
  
  geom_vline(xintercept = 117, size = 1, color = "#000000") +
  geom_vline(xintercept = 130, size = 1, color = "#000000") +
  geom_vline(xintercept = 143, size = 1, color = "#000000") +
  geom_bar(aes(attd_week, corr_bar),stat="identity", fill="#d3d3d3") +
  geom_line(aes(attd_week, change, group=group, color=group), size=2) +
  geom_hline(yintercept=0, size=2) +
  geom_point(aes(attd_week, change), colour = "black", size=1) +
  
  
  scale_colour_manual(values=c("#E69F00","#0072B2")) +
  scale_x_continuous(expand = c(0, 0), limits = c(116, 145), breaks=seq(116,145, by=1), sec.axis = sec_axis(~.+24, breaks=seq(141,168, by=1), name="\nWeek (mortality)\n")) +
  
  xlab("\nWeek (attendance)\n") +
  ylab("\nYoY percentage change\n") +
  
  theme_bw() +
  theme(
    plot.title = element_text(color="black",size=14,face="bold",hjust=0.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.title=element_blank())

corr_mor_plot
```
