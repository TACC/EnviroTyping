# library(tidyverse)
library(magrittr)

df <- read_rds("~/GitHub/EnviroTyping/data/interim/2016/hyb_by_mon_calibr.rds")
table(df$month,df$stat_id)

s0 <- df %>% dplyr::filter(stat_id == 8427) %>% mutate(month = replace(month, between(month,4,8), c(5:9)))
s1 <- df %>% dplyr::filter(stat_id == 8428) %>% mutate(month = replace(month, between(month,3,8), c(5:10)))
s2 <- df %>% dplyr::filter(stat_id == 9078) %>% mutate(month = replace(month, between(month,6,11), c(5:10)))
s3 <- df %>% dplyr::filter(stat_id == 9079) %>% mutate(month = replace(month, between(month,4,9), c(5:10)))
s4 <- df %>% dplyr::filter(stat_id == 9080) %>% mutate(month = replace(month, between(month,4,10), c(5:11)))
s5 <- df %>% dplyr::filter(stat_id %in% c(9082, 9085, 11859)) %>% mutate(month = replace(month, between(month,4,10), c(5:11)))
df <- df %>% dplyr::filter(!(stat_id %in% c(8427, 8428, 9078:9080, 9082, 9085, 11859))) 

df <- df %>% bind_rows(., s0, s1, s2, s3, s4, s5)
table(df$month,df$stat_id)

df %<>% dplyr::filter(between(month, 5, 10))

df1 <- df %>% 
    gather(Var,val,17:56) %>% 
    unite(Var1, Var, month) %>% 
    spread(Var1, val)


write_rds(df1, "~/github/EnviroTyping/data/interim/2016/hyb_by_mon_calib_wide_shifted.rds", compress = "xz")
