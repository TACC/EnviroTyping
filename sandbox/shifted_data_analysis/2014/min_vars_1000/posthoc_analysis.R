library(tidyverse)

setwd("~/github/EnviroTyping/sandbox/shifted_data_analysis/2014/min_vars_1000/")

min_1K_14 <- read_rds("riskProfObj.rds")

df <- read_rds("../../../../data/interim/2014/hyb_by_mon_calib_wide_shifted.rds")

df_clus <- bind_cols(clus = min_1K_14$riskProfClusObj$clustering, df) 

df_clus %>% 
    filter(clus == 2) %>% select(pedi)

outlier <- df_clus %>% 
    filter(exp == "TXH1" & pedi == "Z022E0155/PB80")


