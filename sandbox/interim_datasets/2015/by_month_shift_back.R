library(tidyverse)
library(magrittr)

df <- read_rds("~/github/EnviroTyping/data/interim/2015/hybrid_by_month_calibrated_weather.rds")
table(df$Month,df$StatID)

s0 <- df %>% filter(StatID %in% c(800, 8645, 8650, 8657)) %>% mutate(Month = replace(Month, between(Month,5,10), c(2:7)))
s1 <- df %>% filter(StatID == 1504) %>% mutate(Month = replace(Month, between(Month,5,11), c(1:7)))
s2 <- df %>% filter(StatID == 8427) %>% mutate(Month = replace(Month, between(Month,4,8), c(3:7)))
s3 <- df %>% filter(StatID == 8651) %>% mutate(Month = replace(Month, between(Month,5,8), c(4:7)))
s4 <- df %>% filter(StatID == 8653) %>% mutate(Month = replace(Month, between(Month,5,9), c(3:7)))
df <- df %>% filter(!(StatID %in% c(800, 8645, 8650, 8657, 1504, 8427, 8651, 8653))) 

df <- df %>% bind_rows(.,s0, s1, s2, s3, s4)


#df <- df %>% mutate(Month = replace(Month, between(Month,5,9),c(1:5)))
df <- df %>% filter(Month %in% c(5:9))

table(df$Month,df$StatID)

val <- grep("Median|Min|Max|Mean",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))

df1 <- df %>% select(StatID, Pedi, Repl, Yield, Month, numericVars)
table(df1$Pedi, df1$StatID)

df1 <- df %>% 
    gather(Var,val,17:56) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)



write_rds(df2, "~/GitHub/EnviroTyping/data/interim/2015/hyb_by_mon_cal_wide_shifted_back.rds", compress = "xz")
