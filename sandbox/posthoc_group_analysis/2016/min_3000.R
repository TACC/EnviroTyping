library(amap)
library(cluster)
library(tidyverse)
library(magrittr)
library(lubridate)
library(tictoc)
library(dynamicTreeCut)
library(viridis)
library(gridExtra)
library(reshape2)
library(rlang)
setwd("~/github/EnviroTyping/sandbox/posthoc_group_analysis/2016/")


hyb_mon <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

variance_var <- names(which(map_dbl(hyb_mon[,16:255], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")
hyb_mon_min <- hyb_mon %>% select(pedi, min_vars ) %>% modify_at(1, as_factor) %>% modify_at(1, as.integer) %>% mutate (pedi = pedi - 1)

reduced <- hyb_mon  %>% select(yield) %>% round(digits = 3) %>% as.vector()
post_analysis <- cbind(yield = clusObj$clusObjRunInfoObj$yMat) %>% round(digits = 3) %>% as.vector()
no_outliers <- which(equals(reduced, post_analysis)==TRUE)

clusObj <- read_rds("../../shifted_data_analysis/2016/min_vars_3000_no_outliers/clusObj.rds")
xmat <- clusObj$clusObjRunInfoObj$xMat

create_mapper_na <- function(.p){
    glue::glue("~ ({f_text(.p)}) & !is.na(.)") %>% 
        as.formula() %>%
        as_mapper()
}
na_set <- function(vec, .p) {
    modify_if(vec, create_mapper_na(.p) , ~ NA) %>% 
        reduce(c)
}
replace_to_na_when <- function(tbl, .p) {
    map_df(tbl, ~ na_set(.x, .p) )
} 

hyb_mon_clus <- cbind(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat, clusObj$clusObjRunInfoObj$xMat) 
hyb_mon_clus %<>% replace_to_na_when(~ .x == -999)

cores <- parallel::detectCores()
posthocGroup <- hcluster(hyb_mon_clus[,-2], method = "euclidean", link = "ward", nbproc = cores)



dist <- daisy(hyb_mon_clus[,-2])
tic()
dynamicCutree <- cutreeDynamic(posthocGroup, distM = as.matrix(dist), deepSplit = 4, verbose = 4)
toc()

length(unique(dynamicCutree)) # 14

hyb_mon_groups <- data.frame(group = dynamicCutree, hyb_mon_clus)

# original method to scale data using grid package for multiple plots on same page
l <- list()
t <- list()
for (i in 1:8){
temp <- hyb_mon_groups %>% filter(group == i)
temp <- apply(temp[5:24],2,summary, na.rm = TRUE)
t[[i]] <- temp
data <- rowid_to_column(as.tibble(t(scale(as.tibble(temp))), "ID")
data <- data %>% select(ID, Min = Min., Median, Max = Max.)
df <- melt(data ,  id.vars = 'ID', variable.name = 'series')

p <- ggplot(df, aes(ID,value)) + geom_point(aes(color=series, size = 10), show.legend = FALSE) + labs(title = paste("Scaled Weather Profile for Group",i,sep = " ")) +
    scale_x_continuous(breaks = c(1:20), labels = names(hyb_mon_groups[,c(5:24)])) + theme_bw() +
    theme(axis.text.x = element_text(face="bold", size=6, angle=45,margin = margin(t = 10)), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank())
l[[i]] <- p
}

#pdf("profileByGroup.pdf", paper = "a4r")
grid.arrange(l[[1]],l[[2]], l[[3]],l[[4]],l[[5]],l[[6]], l[[7]], ncol = 2)
#dev.off()

# trying to improve for readability

scale <- hyb_mon_groups %>% 
    group_by(group) %>% 
    summarise_at(4:23, funs(min, max, median), na.rm = TRUE) %>% 
    select(-1) %>% 
    scales::rescale(., to = c(0,10)) %>% 
    as.tibble() %>% 
    cbind(group = seq(1:length(hyb_mon_groups$group)), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scale_this <- function(x){
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}
prescaled <- hyb_mon_groups %>% 
    group_by(group) %>% 
    summarise_at(4:23, funs(min, max, median), na.rm = TRUE) %>% 
    select(-1) 

dews <- prescaled %>% select(str_subset(names(prescaled), "Dew")) %>% normalize() %>% select( -(contains("10")), contains("10"))
humids <- prescaled %>% select(str_subset(names(prescaled), "Humid")) %>% normalize() %>% select( -(contains("10")), contains("10"))
temps <- prescaled %>% select(str_subset(names(prescaled), "Temp")) %>% normalize() %>% select( -(contains("10")), contains("10"))
windDirs <- prescaled %>% select(str_subset(names(prescaled), "windDir")) %>% normalize() %>% select( -(contains("10")), contains("10"))
windGusts <- prescaled %>% select(str_subset(names(prescaled), "windGust")) %>% normalize() %>% select( -(contains("10")), contains("10"))
windSpds <- prescaled %>% select(str_subset(names(prescaled), "windSpd")) %>% normalize() %>% select( -(contains("10")), contains("10"))

scaled <- bind_cols(dews, humids, temps, windDirs, windGusts, windSpds) %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scaled %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Scaled Weather Variables by Group", x = "Weather Variables with Month Number as Suffix") +
    theme(axis.text.x = element_text(face="bold", size = 8, angle = 90, hjust = 1, margin = margin(t = 5)), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE, axis.title.y = element_blank()) +
    scale_color_viridis(discrete = TRUE)

scaled_dew <- dews %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scaled_dew %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Scaled Dew Point Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Scaled Dew Point [C]") +
    theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE) +
    scale_color_viridis(discrete = TRUE)

scaled_humid <- humids %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scaled_humid %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Scaled Humidity Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Scaled Humidity [C]") +
    theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE) +
    scale_color_viridis(discrete = TRUE)

scaled_temp <- temps %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scaled_temp %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Scaled Temperature Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Scaled Temperature [C]") +
    theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE) +
    scale_color_viridis(discrete = TRUE)

scaled_windDir <- windDirs %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scaled_windDir %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Scaled Wind Direction Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Scaled Wind Direction [degrees]") +
    theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE) +
    scale_color_viridis(discrete = TRUE)

scaled_windGust <- windGusts %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scaled_windGust %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Scaled Wind Gust Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Scaled Wind Gust [m/x]") +
    theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE) +
    scale_color_viridis(discrete = TRUE)

scaled_windSpd <- windSpds %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scaled_windSpd %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Scaled Wind Speed Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Scaled Wind Speed [m/s]") +
    theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE) +
    scale_color_viridis(discrete = TRUE)



# looking at the data without scaling by different weather elements ####

dew <- prescaled %>% select(str_subset(names(prescaled), "Dew")) %>% select( -(contains("10")), contains("10"))
humid <- prescaled %>% select(str_subset(names(prescaled), "Humid")) %>% select( -(contains("10")), contains("10"))
temp <- prescaled %>% select(str_subset(names(prescaled), "Temp"))  %>% select( -(contains("10")), contains("10"))
windDir <- prescaled %>% select(str_subset(names(prescaled), "windDir"))  %>% select( -(contains("10")), contains("10"))
windGust <- prescaled %>% select(str_subset(names(prescaled), "windGust"))  %>% select( -(contains("10")), contains("10"))
windSpd <- prescaled %>% select(str_subset(names(prescaled), "windSpd"))  %>% select( -(contains("10")), contains("10"))


dew2 <- dew %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

dew2 %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Dew Point Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Dew Point [C]") +
    theme(axis.text.x = element_text( face="bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)+
    scale_color_viridis(discrete = TRUE)

humid2 <- humid %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

humid2 %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Humidity Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Relative Humidity [%]") +
    theme(axis.text.x = element_text(face="bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)+
    scale_color_viridis(discrete = TRUE)

temp2 <- temp %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

temp2 %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Temperature Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Temperature [C]") +
    theme(axis.text.x = element_text(face="bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)+
    scale_color_viridis(discrete = TRUE)

windDir2 <- windDir %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

windDir2 %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Wind Direction Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Wind Direction [degrees]") +
    theme(axis.text.x = element_text(face="bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)+
    scale_color_viridis(discrete = TRUE)

windGust2 <- windGust %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

windGust2 %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Wind Gust Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Wind Gust [m/s]") +
    theme(axis.text.x = element_text(face="bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)+
    scale_color_viridis(discrete = TRUE)


windSpd2 <- windSpd %>% 
    cbind(group = seq(1:8), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

windSpd2 %>% ggplot(aes(fct_inorder(factor(var)), value)) +
    geom_point(aes(color = stat), size = 5) +
    facet_wrap(~ group, scales = "free_y") + 
    labs(title = "Wind Speed Profile by Group", x = "Weather Variables with Month Number as Suffix", y = "Wind Speed [m/s]") +
    theme(axis.text.x = element_text(face="bold", size = 10, angle = 45), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92",size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"), legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)+
    scale_color_viridis(discrete = TRUE)


pdf("Post-Hoc Analysis of Weather Profiles by Additional Grouping.pdf", paper = "a4r", width = 10, height = 7)
dev.off()
