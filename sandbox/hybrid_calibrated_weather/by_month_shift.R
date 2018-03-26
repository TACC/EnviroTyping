library(tidyverse)
library(lubridate)

df <- read_csv("data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.csv")
df1 <- df %>% mutate(Month = month(Date))
table(df$Month,df$StatID)

df <- read_csv("data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.csv")
table(df$Month,df$StatID)
s3 <- df %>% filter(StatID==8428) %>% mutate(Month = replace(Month, between(Month,3,7), c(5:9)))
s4 <- df %>% filter(StatID %in% c(8427,8658)) %>% mutate(Month = replace(Month, between(Month,4,8), c(5:9)))
s5 <- df %>% filter(StatID==8658) %>% mutate(Month = replace(Month, between(Month,4,8), c(5:9)))
df <- df %>% filter(!(StatID %in% c(8248,8427,8657))) 
df <- df %>% filter(StatID!=8427)
df <- df %>% filter(StatID!=8658)
df <- df %>% bind_rows(.,s3,s4)

                    