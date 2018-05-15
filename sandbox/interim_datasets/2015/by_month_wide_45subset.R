library(tidyverse)
setwd("~/GitHub/EnviroTyping/")

df <- read_rds("data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.rds")
val <- grep("Min|Max|Mean|Median",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))

df1 <- df %>% select(1:16,numericVars)
table(df1$StatID,df1$Month)

df2 <- df1 %>% 
    gather(Var,val,17:53) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)

uniqHybrids <- df2 %>% group_by(Pedi) %>% 
    summarise(count = n_distinct(Exp)) %>% 
    filter(count == 11) %>% 
    pull(Pedi)

df3 <- df2 %>% filter(Pedi %in% uniqHybrids)


write_rds(df3, "data/interim/G2F_Hybrid/hyb_by_mon_calib_45subset_all_stats.rds", compress = "xz")


# Using min and max weather covariates

val <- grep("Min|Max",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))

df1 <- df %>% select(1:16,numericVars)
table(df1$StatID,df1$Month)

df2 <- df1 %>% 
    gather(Var,val,17:34) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)

uniqHybrids <- df2 %>% group_by(Pedi) %>% 
    summarise(count = n_distinct(Exp)) %>% 
    filter(count == 11) %>% 
    pull(Pedi)

df3 <- df2 %>% filter(Pedi %in% uniqHybrids)

write_rds(df3, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_45subset_162covMinMax.rds", compress = "xz")
