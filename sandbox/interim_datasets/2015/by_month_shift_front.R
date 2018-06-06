library(tidyverse)
library(lubridate)
library(data.table)

df <- read_rds("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.rds")
table(df$Month,df$StatID)


s3 <- df %>% filter(StatID == 8428) %>% mutate(Month = replace(Month, between(Month,3,7), c(5:9)))
s4 <- df %>% filter(StatID == 8427) %>% mutate(Month = replace(Month, between(Month,4,8), c(5:9)))
s5 <- df %>% filter(StatID == 8658) %>% mutate(Month = replace(Month, between(Month,4,7), c(5:8)))
df <- df %>% filter(!(StatID %in% c(8428,8427,8658,8651))) 

df <- df %>% bind_rows(.,s3,s4)


#df <- df %>% mutate(Month = replace(Month, between(Month,5,9),c(1:5)))
df <- df %>% filter(Month %in% c(5:9))

table(df$Month,df$StatID)

val <- grep("Median|Min|Max|Mean",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))

df1 <- df %>% select(StatID, Pedi, Repl, Yield, Month, numericVars)
table(df1$Pedi, df1$StatID)

df2 <- df1 %>% 
    gather(Var,val,6:42) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)



write_rds(df2, "~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hybrid_by_month_shift_all_stats.rds", compress = "xz")
