# library(tidyverse)
library(lubridate)
library(magrittr)

df <- read_rds("~/GitHub/EnviroTyping/data/interim/2016/hyb_by_mon_calibr.rds")
table(df$Month,df$StatID)


s1 <- df %>% dplyr::filter(StatID == 8428) %>% mutate(Month = replace(Month, between(Month,3,8), c(4:9)))
s2 <- df %>% dplyr::filter(StatID == 8652) %>% mutate(Month = replace(Month, between(Month,5,11), c(4:10)))
s3 <- df %>% dplyr::filter(StatID == 9078) %>% mutate(Month = replace(Month, between(Month,6,11), c(5:10)))
s4 <- df %>% dplyr::filter(StatID %in% c(10794, 10795)) %>% mutate(Month = replace(Month, between(Month,5,11), c(4:10)))
df <- df %>% dplyr::filter(!(StatID %in% c(8428,8652,9078,10794:10795))) 

df <- df %>% bind_rows(.,s1, s2, s3, s4)
table(df$Month,df$StatID)

df1 <- df %>% 
    gather(Var,val,17:56) %>% 
    unite(Var1, Var, Month) %>% 
    spread(Var1, val)

na_cols <- str_subset(names(df1), "NA")

df1 %<>% select(-one_of(na_cols))

write_rds(df1, "~/GitHub/EnviroTyping/data/interim/2016/hyb_by_mon_calib_wide_shifted.rds", compress = "xz")
