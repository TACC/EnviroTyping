# library(tidyverse)
library(magrittr)

df <- read_rds("~/GitHub/EnviroTyping/data/interim/2016/hyb_by_mon_calibr.rds")
table(df$Month,df$StatID)

s0 <- df %>% dplyr::filter(StatID == 8427) %>% mutate(Month = replace(Month, between(Month,4,8), c(5:9)))
s1 <- df %>% dplyr::filter(StatID == 8428) %>% mutate(Month = replace(Month, between(Month,3,8), c(5:10)))
s2 <- df %>% dplyr::filter(StatID == 9078) %>% mutate(Month = replace(Month, between(Month,6,11), c(5:10)))
s3 <- df %>% dplyr::filter(StatID == 9079) %>% mutate(Month = replace(Month, between(Month,4,9), c(5:10)))
s4 <- df %>% dplyr::filter(StatID == 9080) %>% mutate(Month = replace(Month, between(Month,4,10), c(5:11)))
s5 <- df %>% dplyr::filter(StatID %in% c(9082, 9085, 11859)) %>% mutate(Month = replace(Month, between(Month,4,10), c(5:11)))
df <- df %>% dplyr::filter(!(StatID %in% c(8427, 8428, 9078:9080, 9082, 9085, 11859))) 

df <- df %>% bind_rows(., s0, s1, s2, s3, s4, s5)
table(df$Month,df$StatID)

df %<>% dplyr::filter(between(Month, 5, 10))

df1 <- df %>% 
    gather(Var,val,17:56) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)


write_rds(df1, "~/github/EnviroTyping/data/interim/2016/hyb_by_mon_calib_wide_shifted.rds", compress = "xz")
