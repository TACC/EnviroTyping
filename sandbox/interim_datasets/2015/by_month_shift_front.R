library(tidyverse)

df <- read_rds("~/github/EnviroTyping/data/interim/2015/hyb_by_mon_calib.rds")
table(df$month,df$stat_id)


s3 <- df %>% filter(stat_id == 8428) %>% mutate(month = replace(month, between(month,3,7), c(5:9)))
s4 <- df %>% filter(stat_id == 8427) %>% mutate(month = replace(month, between(month,4,8), c(5:9)))
s5 <- df %>% filter(stat_id == 8658) %>% mutate(month = replace(month, between(month,4,7), c(5:8)))
df <- df %>% filter(!(stat_id %in% c(8428,8427,8658,8651))) 

df <- df %>% bind_rows(.,s3,s4)


#df <- df %>% mutate(month = replace(month, between(month,5,9),c(1:5)))
df <- df %>% filter(month %in% c(5:10))

table(df$month,df$stat_id)

# df1 <- df %>% select(stat_id, pedi, repl, yield, month, numeric_vars)
# table(df1$Pedi, df1$stat_id)

df2 <- df %>% 
    gather(Var,val,17:56) %>% 
    unite(Var1, Var, month) %>% 
    spread(Var1, val)

# check for NA's
na.s <- df2 %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(df2, "~/GitHub/EnviroTyping/data/interim/2015/hyb_by_mon_calib_wide_shifted.rds", compress = "xz")
