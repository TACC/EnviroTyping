library(tidyverse)

df <- read_rds("~/github/EnviroTyping/data/interim/2014/hyb_by_mon_calib.rds")

df1 <- df %>% 
    gather(Var,val,17:48) %>% 
    unite(Var1, Var, month) %>% 
    spread(Var1, val)

col <- names(df1)[16:335]
has_variance <- names(map_dbl(df1[col], var, na.rm = TRUE) != 0)


write_rds(df1, "~/github/EnviroTyping/data/interim/2014/hyb_by_mon_calib_wide.rds", compress = "xz")


df1 %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))
