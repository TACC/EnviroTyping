# library(tidyverse)
library(magrittr)

df <- read_rds("~/GitHub/EnviroTyping/data/interim/2014/hyb_by_mon_calib.rds")
table(df$month,df$stat_id)

s0 <- df %>% dplyr::filter(stat_id == 8427) %>% mutate(month = replace(month, between(month,4,9), c(5:10)))
s1 <- df %>% dplyr::filter(stat_id == 8428) %>% mutate(month = replace(month, between(month,3,8), c(5:10)))

df <- df %>% dplyr::filter(!(stat_id %in% c(8427:8428))) 

df <- df %>% bind_rows(., s0, s1)
df %<>% dplyr::filter(between(month, 5, 10))
table(df$month,df$stat_id)

df1 <- df %>% 
    gather(Var,val,17:48) %>% 
    unite(Var1, Var, month) %>% 
    spread(Var1, val)


write_rds(df1, "~/github/EnviroTyping/data/interim/2014/hyb_by_mon_calib_wide_shifted.rds", compress = "xz")
