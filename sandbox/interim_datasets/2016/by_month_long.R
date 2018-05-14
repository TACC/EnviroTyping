library(tidyverse)
library(lubridate)
library(magrittr)

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_G2F_2016_Data_Mar_2018/a._2016_hybrid_phenotypic_data/g2f_2016_hybrid_data_no_outliers.csv",col_types = cols("Date Plot Planted" = col_date("%m/%d/%Y"), "Date Plot Harvested" = col_date("%m/%d/%Y"), "Plant Height [cm]" = col_number(), "Ear Height [cm]" = col_number()))

wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_G2F_2016_Data_Mar_2018/c._2016_weather_data/g2f_2016_weather_calibrated.csv")

meta <- read_csv("data/external/G2F/g2f_2016_field_metadata.csv")

meta <- meta %>% select(Exp = "Experiment Code", City, Lat = "Weather station latitude (in decimal numbers NOT DMS)", Lon = "Weather station longitude (in decimal numbers NOT DMS)")

##### Month #####
# tidy the data
wthmon <- wth %>% 
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Month, Year, Temp = "Calibrated Temperature [C]", Dew = "Calibrated Dew Point [C]", Humid = "Calibrated Relative Humidity [%]", Solar = "Solar Radiation [W/m2]", Rain = "Rainfall [mm]", windSpd = "Calibrated Wind Speed [m/s]", windDir = "Calibrated Wind Direction [degrees]", windGust = "Calibrated Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%VWC]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Year, Month) %>% 
    
    # changing the sort    
    arrange(Exp, StatID, Year, Month )

# converts the weather variables to numeric
cols <- names(wthmon)[5:14]
wthmon %<>% mutate_at(cols,funs(as.numeric(.)))

# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wthmon <- wthmon %>% summarise_if(is.numeric, funs(min, max, mean, median), na.rm = TRUE)

# Split Exp with multiple sites
wthmon <- wthmon %>% 
    ungroup(Exp) %>% 
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>% 
    unnest(Exp) %>% 
    select(Exp, everything()) %>% 
    drop_na(Exp)

wthmon$Exp[wthmon$Exp == ""] <- "NA"

wthmon <- wthmon %>% filter(Exp != "NA")

wthmon <- left_join(wthmon, meta, by = "Exp")

# tidy the data
hyb <- hyb %>% 
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = "Replicate", Planted = "Date Plot Planted",Harvest = "Date Plot Harvested",plantHt = "Plant Height [cm]", earHt = "Ear Height [cm]",testWt = "Test Weight [lbs/bu]",plotWt = "Plot Weight [lbs]", Yield = "Grain Yield [bu/acre]") %>% 
    
    # changing the sort 
    arrange(Exp, Pedi, Repl)

##### Month #####
# joining the tidy weather data with min/max variables
# right join to preserve weather data and fill matching hybrid data to each expermient
hybmon <- left_join(hyb, wthmon, by = "Exp") %>%  
    drop_na(Yield) %>% 
    select(1:5, 11:13, 54:56, 6:10, 14:53)

# check for NA's
na.s <- hybmon %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybmon, "data/interim/2016/hyb_by_mon_calibr.rds")


