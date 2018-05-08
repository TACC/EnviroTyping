library(tidyverse)
setwd("~/GitHub/EnviroTyping/")

df <- read_rds("data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.rds")
val <- grep("Median",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))

df1 <- df %>% select(1:16,numericVars)
table(df1$StatID,df1$Month)

uniqHybrids <- df1 %>% group_by(Pedi) %>%
    summarise(count = n_distinct(Exp)) %>%
    filter(count == 11) %>%
    pull(Pedi)

df2 <- df1 %>% filter(Pedi %in% uniqHybrids)

write_rds(df2, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather_long_45subset.rds", compress = "xz")
