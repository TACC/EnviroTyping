library(amap)
library(cluster)
library(tidyverse)
library(magrittr)
library(dynamicTreeCut)
library(viridis)
library(rlang)
#############################################################################################################################
#                                             CHANGE PATH TO YOUR FILE                                                      #
################VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV###################################
path_to_clusObj <- "~/github/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000_no_outliers/clusObj.rds"
path_to_original_data <- "~/github/EnviroTyping/data/interim/2016/hyb_by_mon_calib_wide_shifted.rds"
#############################################################################################################################
og_dat <- read_rds(path_to_original_data)
clusObj <- read_rds(path_to_clusObj)
#############################################################################################################################

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

dynamicCutree <- cutreeDynamic(posthocGroup, distM = as.matrix(dist), deepSplit = 4, verbose = 4)


num_groups <- n_distinct(dynamicCutree)

hyb_mon_groups <- data.frame(group = dynamicCutree, hyb_mon_clus)


normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

prescaled <- hyb_mon_groups %>% 
    group_by(group) %>% 
    summarise_at(4:(length(.) - 1), funs(min, max, median), na.rm = TRUE) %>% 
    select(-1) 

dews <- prescaled %>% select(str_subset(names(prescaled), "dew|Dew")) %>% normalize() %>% select( -(contains("10")), contains("10"))
humids <- prescaled %>% select(str_subset(names(prescaled), "humid|Humid")) %>% normalize() %>% select( -(contains("10")), contains("10"))
temps <- prescaled %>% select(str_subset(names(prescaled), "temp|Temp")) %>% normalize() %>% select( -(contains("10")), contains("10"))
windDirs <- prescaled %>% select(str_subset(names(prescaled), "wind_dir|windDir")) %>% normalize() %>% select( -(contains("10")), contains("10"))
windGusts <- prescaled %>% select(str_subset(names(prescaled), "wind_gust|windGust")) %>% normalize() %>% select( -(contains("10")), contains("10"))
windSpds <- prescaled %>% select(str_subset(names(prescaled), "wind_spd|windSpd")) %>% normalize() %>% select( -(contains("10")), contains("10")) 


scaled <- bind_cols(dews, humids, temps, windDirs, windGusts, windSpds) %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
scaled_dew <- dews %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
scaled_humid <- humids %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
scaled_temp <- temps %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
scaled_windDir <- windDirs %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
scaled_windGust <- windGusts %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
scaled_windSpd <- windSpds %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

dew <- prescaled %>% select(str_subset(names(prescaled), "dew|Dew")) %>% select( -(contains("10")), contains("10"))
humid <- prescaled %>% select(str_subset(names(prescaled), "humid|Humid")) %>% select( -(contains("10")), contains("10"))
temp <- prescaled %>% select(str_subset(names(prescaled), "temp|Temp"))  %>% select( -(contains("10")), contains("10"))
windDir <- prescaled %>% select(str_subset(names(prescaled), "wind_dir|windDir"))  %>% select( -(contains("10")), contains("10"))
windGust <- prescaled %>% select(str_subset(names(prescaled), "wind_gust|windGust"))  %>% select( -(contains("10")), contains("10"))
windSpd <- prescaled %>% select(str_subset(names(prescaled), "wind_spd|windSpd"))  %>% select( -(contains("10")), contains("10"))


dew2 <- dew %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
humid2 <- humid %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
temp2 <- temp %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
windDir2 <- windDir %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
windGust2 <- windGust %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
windSpd2 <- windSpd %>% 
    cbind(group = seq(1:num_groups), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")


# merging hybrid labels from original data to premium levels output



reduced <- og_dat  %>% select(contains("yield", ignore.case = TRUE)) %>% round(digits = 3) %>% pull()
post_analysis <- clusObj$clusObjRunInfoObj$yMat %>% round(digits = 3) %>% as.vector()


outliers <- vector(mode = "integer")
outliers <- c(outliers, which(reduced != post_analysis)[1])
diff <- length(reduced) - length(post_analysis) - 1
if(diff > 0) {
for (i in 1:diff) {
  outliers <- c(outliers, which(reduced[-outliers] != post_analysis)[1])
}
hybrids_groups <- data_frame(hybrids = og_dat$Pedi[-outliers], group = hyb_mon_groups$group)
} else {
    hybrids_groups <- data_frame(hybrids = og_dat$Pedi, group = hyb_mon_groups$group)
}
