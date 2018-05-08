library(tidyverse)
setwd("~/GitHub/EnviroTyping/")

df <- read_rds("data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.rds")
val <- grep("Median",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))

df1 <- df %>% select(1:16,numericVars)
table(df1$StatID,df1$Month)

df2 <- df1 %>% 
    gather(Var,val,17:25) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)

uniqHybrids <- df2 %>% group_by(Pedi) %>% 
    summarise(count = n_distinct(Exp)) %>% 
    filter(count == 11) %>% 
    pull(Pedi)

df3 <- df2 %>% filter(Pedi %in% uniqHybrids)


write_rds(df3, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_45subset_81covMinMax.rds", compress = "xz")


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
