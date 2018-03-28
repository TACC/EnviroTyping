# imports all the packages i'll need
library(tidyverse)

setwd("~/GitHub/EnviroTyping/")
# read the raw data as a tibble/data.frame
hyb <-  read_csv("data/external/G2F from Cyverse DataStore/g2f_2015_hybrid_data_no_outliers.csv",col_types = cols("Date Planted" = col_date("%m/%d/%Y"), "Date Harvested" = col_date("%m/%d/%Y"), "Plant height [cm]" = col_number(), "Ear height [cm]" = col_number()))


# tidy the data
hyb <- hyb %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Planted = "Date Planted",Harvest = "Date Harvested",plantHt = "Plant height [cm]", earHt = "Ear height [cm]",testWt = "Test weight [lbs]",plotWt = "Plot Weight [lbs]", Yield = "Grain yield [bu/acre]") %>% 
    
    # changing the sort 
    arrange(Exp, Pedi, Repl)

##### Day #####
# joining the tidy weather data with min/max variables
# right join to preserve weather data and fill matching hybrid data to each expermient
hybday <- right_join(hyb, wthday, by = "Exp") %>%  
    drop_na(Yield) %>% 
    select(1:5, 11:12, 53:55, 6:10, 13:52)

# check for NA's
hybday %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))


# write the data to csv and rds
write_csv(hybday, "data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.csv")
write_rds(hybday, "data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.rds", compress = "xz")

##### Week #####
# joining the tidy weather data with min/max variables
# right join to preserve weather data and fill matching hybrid data to each expermient
hybwk <- right_join(hyb, wthwk, by = "Exp") %>%  
    drop_na(Yield) %>% 
    select(1:5, 11:13, 54:56, 6:10, 14:53)

# check for NA's
hybwk %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))


# write the data to csv and rds
write_csv(hybwk, "data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.csv")
write_rds(hybwk, "data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.rds", compress = "xz")

##### Month #####
# joining the tidy weather data with min/max variables
# right join to preserve weather data and fill matching hybrid data to each expermient
hybmon <- right_join(hyb, wthmon, by = "Exp") %>%  
    drop_na(Yield) %>% 
    select(1:5, 11:13, 54:56, 6:10, 14:53)

# check for NA's
hybmon %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))


# write the data to csv and rds
write_csv(hybmon, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.csv")
write_rds(hybmon, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.rds", compress = "xz")

# checkr <- read_rds("data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.rds")
# checkc <- read_csv("data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.csv")
# identical(hybwk, checkr)
