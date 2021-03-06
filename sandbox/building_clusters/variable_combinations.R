# find all possible combinations of weather covariates
library(tidyverse)

setwd("~/github/EnviroTyping/sandbox/building_clusters/output")

df <- read_rds("~/github/EnviroTyping/data/interim/2015/hyb_by_mon_calib_wide_shifted.rds")

# finding unique weather variables
mean_vars <- str_subset(names(df), "mean")
var_names <- unique(str_replace(mean_vars, "_mean.*", ""))

# big list of all the combinations of weather covariates
var_comb <- list()
for (i in 1:(length(var_names) - 1)){
    var_comb[[i]] <- as_data_frame(combn(var_names, i))
}

# thinking of ways to pass variables as lists easily to premium
length(big_list)
big_list <- list()
temp_list <- list()
for (i in seq_along(var_comb)){
    temp_list[[i]] <- mutate_all(var_comb[[i]], as.list) %>% as.list
    big_list <- c(big_list, temp_list)
}

