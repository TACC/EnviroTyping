library(tidyverse)
library(maps)
library(mapdata)

min_max_scale <- function(x){
    return ((x - min(x)) / (max(x) - min(x)))
}

hyb_by_mon_posthoc_map = read_rds("~/RProjects/Maps/hyb_by_mon_posthoc_map.rds")

# Tidy locations in Iowa
hyb_by_mon_posthoc_map$Lat[hyb_by_mon_posthoc_map$Exp == "IAH1"] = 41.99762
hyb_by_mon_posthoc_map$Lon[hyb_by_mon_posthoc_map$Exp == "IAH1"] = -93.69622
hyb_by_mon_posthoc_map$Lat[hyb_by_mon_posthoc_map$Exp == "IAH2"] = 42.06750
hyb_by_mon_posthoc_map$Lon[hyb_by_mon_posthoc_map$Exp == "IAH2"] = -93.61800   
hyb_by_mon_posthoc_map$Lat[hyb_by_mon_posthoc_map$Exp == "IAH3"] = 41.97589
hyb_by_mon_posthoc_map$Lon[hyb_by_mon_posthoc_map$Exp == "IAH3"] = -92.24096   
hyb_by_mon_posthoc_map$Lat[hyb_by_mon_posthoc_map$Exp == "IAH4"] = 41.19938
hyb_by_mon_posthoc_map$Lon[hyb_by_mon_posthoc_map$Exp == "IAH4"] = -91.49214   

# Load map data

usa = map_data("usa")
states = map_data("state")
canada = map_data("world","canada")
stations = hyb_by_mon_posthoc_map %>% select(Exp, "lat" = Lat, "long" = Lon) %>% distinct()

# Adding Iowa geo info from 2015 to station data

stations$lat[6:9] = c(41.99762,42.06750,41.97589,41.19938)
stations$long[6:9] = c(-93.69622,-93.61800,-92.24096,-91.49214)

dot_size_Exp = hyb_by_mon_posthoc_map %>% group_by(Exp) %>% 
    summarize(Mean = mean(Yield,na.rm = TRUE)) %>%
    mutate(ln_Mean = log(Mean),min_max_Mean = 3*abs(min_max_scale(Mean))+1,scale_Mean = abs(scale(Mean))) # 4 is arbitrary; any scalar can be chosen

# Filter by group

group1 = hyb_by_mon_posthoc_map %>% filter(group==1) %>% select(Exp,Pedi,Yield,clus,Lat,Lon)
group2 = hyb_by_mon_posthoc_map %>% filter(group==2) %>% select(Exp,Pedi,Yield,clus,Lat,Lon)
group3 = hyb_by_mon_posthoc_map %>% filter(group==3) %>% select(Exp,Pedi,Yield,clus,Lat,Lon)
group4 = hyb_by_mon_posthoc_map %>% filter(group==4) %>% select(Exp,Pedi,Yield,clus,Lat,Lon)

# Summarize data and add variables to describe scaling to later use in making of dot sizes

group1_mean = group1 %>% group_by(Exp,Pedi) %>% 
    summarize(Mean = mean(Yield,na.rm = TRUE))
group1_mean = left_join(group1_mean,stations,by = NULL) %>%
    arrange(desc(Mean)) %>% 
    mutate(ln_Mean = log(Mean),min_max_Mean = 3*abs(min_max_scale(Mean))+1,scale_Mean = abs(scale(Mean)))

group2_mean = group2 %>% group_by(Exp,Pedi) %>% 
    summarize(Mean = mean(Yield,na.rm = TRUE)) 
group2_mean = left_join(group2_mean,stations,by = NULL) %>%
    arrange(desc(Mean)) %>%
    mutate(ln_Mean = log(Mean),min_max_Mean = 3*abs(min_max_scale(Mean))+1,scale_Mean = abs(scale(Mean)))

group3_mean = group3 %>% group_by(Exp,Pedi) %>% 
    summarize(Mean = mean(Yield,na.rm = TRUE))
group3_mean = left_join(group3_mean,stations,by = NULL) %>%
    arrange(desc(Mean)) %>% 
    mutate(ln_Mean = log(Mean),min_max_Mean = 3*abs(min_max_scale(Mean))+1,scale_Mean = abs(scale(Mean)))

group4_mean = group4 %>% group_by(Exp,Pedi) %>% 
    summarize(Mean = mean(Yield,na.rm = TRUE))
group4_mean = left_join(group4_mean,stations,by = NULL) %>%
    arrange(desc(Mean)) %>% 
    mutate(ln_Mean = log(Mean),min_max_Mean = 3*abs(min_max_scale(Mean))+1,scale_Mean = abs(scale(Mean)))

# Combine the top and bottom performers for each group 

top_bottom_1 = rbind(head(group1_mean),tail(group1_mean))
top_bottom_2 = rbind(head(group2_mean),tail(group2_mean))
top_bottom_3 = rbind(head(group3_mean),tail(group3_mean))
top_bottom_4 = rbind(head(group4_mean),tail(group4_mean))

# Plot by Experiment Location and Yield Size

# 29 at most missing IA, NE, NY, SC, TXH2
# need different package for canada data
# unique(hyb_by_mon_calib_wide_shifted$Exp)
# all = unique(g2f_2016_field_metadata$`Experiment Code`) 

ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
    #geom_polygon(data = canada, aes(x = long, y = lat, group = group), color = "white") +
    geom_point(data = stations, aes(x = long, y = lat, color = dot_size_Exp$Mean), size = dot_size_Exp$min_max_Mean) + 
    scale_color_gradient(low = "red", high = "deepskyblue3",name = "Average\nYield") +
    guides(fill=guide_legend(title="Average Yield")) + 
    labs(title = "Distribution of Crop Experiments", x = "Longitude", y = "Latidude") +
    theme(plot.title = element_text(hjust=0.5)) +
    coord_fixed(1.4)

# Plot by location of Groups with group color coding; sizes based upon ln(Mean)

ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
    geom_point(data = top_bottom_1, aes(x = long, y = lat, color = "#019E73"), size = top_bottom_1$min_max_Mean, position = position_jitter(w = 1, h = 0.8)) + 
    geom_point(data = top_bottom_2, aes(x = long, y = lat, color = "#57B4E9"), size = top_bottom_2$min_max_Mean, position = position_jitter(w = 1, h = 0.8)) +    
    geom_point(data = top_bottom_3, aes(x = long, y = lat, color = "#E69F00"), size = top_bottom_3$min_max_Mean, position = position_jitter(w = 1, h = 0.8)) + 
    geom_point(data = top_bottom_4, aes(x = long, y = lat, color = "#F0E442"), size = top_bottom_4$min_max_Mean, position = position_jitter(w = 1, h = 0.8)) +
    scale_color_discrete(labels=c("Group 1","Group 2","Group 3","Group 4"),name = "Group") +
    guides(fill=guide_legend(title="Group")) + 
    labs(title = "Locations of Select Hybrids\nby Post-hoc Group", x = "Longitude", y = "Latidude") +
    theme(plot.title = element_text(hjust=0.5)) +
    coord_fixed(1.4)

# Plot of best and worst performers in Group 1 - fix number of plots

ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
    geom_point(data = group1_mean, aes(x = long, y = lat, color = group1_mean$Mean), size = group1_mean$min_max_Mean, position = "jitter") + 
    scale_color_gradient(low = "red", high = "deepskyblue3",name = "Average\nYield") +
    guides(fill=guide_legend(title="Average Yield")) + 
    labs(title = "Top and Worst Performers\nGroup 1", x = "Longitude", y = "Latidude") +
    theme(plot.title = element_text(hjust=0.5)) +
    coord_fixed(1.4)
