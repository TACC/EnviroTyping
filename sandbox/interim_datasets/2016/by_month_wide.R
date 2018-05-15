library(tidyverse)
setwd("~/GitHub/EnviroTyping/")
df <- read_rds("data/interim/2016/hyb_by_mon_calibr.rds")
val <- grep("mean|median|min|max",names(df))
numericVars <- names(which(map_dbl(df[val], var, na.rm = TRUE) != 0))
df %<>% select(1:16,numericVars)
df1 <- df %>% 
    gather(Var,val,17:35) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)
str(df1)

write_rds(df1, "data/interim/2016/hyb_by_mon_calib_wide.rds", compress = "xz")
