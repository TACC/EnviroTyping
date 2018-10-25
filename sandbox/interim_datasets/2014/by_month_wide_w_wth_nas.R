library(tidyverse)

df <- read_rds("~/Github/EnviroTyping/data/interim/2014/hyb_by_mon_calib_w_wth_nas.rds")

val <- grep("Min|Max",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))


df1 <- df %>% 
    gather(Var,val,17:56) %>% 
    unite(Var1, Var, month) %>% 
    spread(Var1, val)

df2 <- df1 %>% select(-contains("NA"))

write_rds(df2, "~/Github/EnviroTyping/data/interim/2014/hyb_by_mon_calib_wide_w_wth_nas.rds", compress = "xz")
