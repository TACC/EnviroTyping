library(tidyverse)
setwd("~/GitHub/EnviroTyping/")
df <- read_rds("data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.rds")
val <- grep("Min|Max",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))
df1 <- df %>% select(1:16,numericVars) %>% 
    filter(between(Month,5,7))
table(df1$StatID,df1$Month)
df2 <- df1 %>% 
    gather(Var,val,contains("Min")) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)

df3 <- df1 %>% 
    gather(Var,val,17:34) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)

write_rds(df3, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather_wide_months_5-7.rds", compress = "xz")
