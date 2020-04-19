library(tidyverse)
library(lubridate)

# Weather Data
wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/c._2015_weather_data/g2f_2015_weather_calibrated.csv")

# Meta Data
meta <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/z._2015_supplemental_info/g2f_2015_field_metadata.csv")

# Hybrid Data

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/a._2015_hybrid_phenotypic_data/g2f_2015_hybrid_data_no_outliers.csv")

meta <- meta %>% select(Exp = "Experiment", City, Lat = "WS Lat", Lon = "WS Lon")

wthday <- wth %>%
    
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Day, Month, Year, DoY = "Day of Year", Temp = "Calibrated Temperature [C]",
           Dew = "Calibrated Dew Point [C]", Humid = "Calibrated Relative Humidity [%]", Solar = "Solar Radiation [W/m2]",
           Rain = "Rainfall [mm]", windSpd = "Calibrated Wind Speed [m/s]", windDir = "Calibrated Wind Direction [degrees]",
           windGust = "Calibrated Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%]") %>%
    
    # get the week since the start of the year
    mutate(Date = make_date(Year, Month, Day)) %>%
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Date) %>%
    
    # changing the sort    
    arrange(Exp, StatID, Date) %>%
    
    # removes NA's
    drop_na()


wthday <- wthday %>%
    summarise(tempMin = min(Temp), tempMax = max(Temp), tempMean = mean(Temp), tempMedian = median(Temp),
              dewMin = min(Dew), dewMax = max(Dew), dewMean = mean(Dew), dewMedian = median(Dew),
              humidMin = min(Humid), humidMax = max(Humid), humidMean = mean(Humid), humidMedian = median(Humid),
              solarMin = min(Solar), solarMax = max(Solar), solarMean = mean(Solar), solarMedian = median(Solar),
              rainMin = min(Rain), rainMax = max(Rain), rainMean = mean(Rain), rainMedian = median(Rain),
              windSpdMin = min(windSpd), windSpdMax = max(windSpd), windSpdMean = mean(windSpd), windSpdMedian = median(windSpd),
              windDirMin = min(windDir), windDirMax = max(windDir), windDirMean = mean(windDir), windDirMedian = median(windDir),
              windGustMin = min(windGust), windGustMax = max(windGust), windGustMean = mean(windGust),
              windGustMedian = median(windGust),
              soilTempMin = min(soilTemp), soilTempMax = max(soilTemp), soilTempMean = mean(soilTemp),
              soilTempMedian = median(soilTemp),
              soilMoistMin = min(soilMoist), soilMoistMax = max(soilMoist), soilMoistMean = mean(soilMoist),
              soilMoistMedian = median(soilMoist))


wthday <- wthday %>%
    ungroup(Exp) %>%
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>%
    unnest(Exp) %>%
    select(Exp, everything())

wthday$Exp[wthday$Exp == ""] <- "NA"

wthday <- wthday %>% filter(Exp != "NA")

wthday <- inner_join(wthday, meta, by = "Exp")

hyb <- hyb %>%
    
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Planted = "Date Planted",Harvest = "Date Harvested",plantHt = "Plant height [cm]", earHt = "Ear height [cm]",testWt = "Test weight [lbs]",plotWt = "Plot Weight [lbs]", Yield = "Grain yield [bu/acre]") %>%
    
    # changing the sort
    arrange(Exp, Pedi, Repl)

hybday <- right_join(hyb, wthday, by = "Exp") %>%  
    drop_na(Yield) %>%
    select(1:5, 11:12, 53:55, 6:10, 13:52)

hybday %>%
    select_if(function(x) any(is.na(x))) %>%
    summarise_all(funs(sum(is.na(.))))

write_csv(hybday, "data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.csv")
write_rds(hybday, "data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.rds", compress = "xz")

checkr <- read_rds("data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.rds")
checkc <- read_csv("data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.csv")
identical(hybday, checkr)



###########################################
############################################
##############################################
#############################################
############################################

library(magrittr)

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/a._2015_hybrid_phenotypic_data/g2f_2015_hybrid_data_no_outliers.csv",col_types = cols("Date Planted" = col_date("%m/%d/%Y"), "Date Harvested" = col_date("%m/%d/%Y"), "Plant height [cm]" = col_number(), "Ear height [cm]" = col_number()))

wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/c._2015_weather_data/g2f_2015_weather_calibrated.csv")

meta <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/z._2015_supplemental_info/g2f_2015_field_metadata.csv") %>% select(exp = "Experiment", city = "City", lat = "WS Lat", lon = "WS Lon")

##### Month #####
# tidy the data
wthmon <- wth %>% 
    # choosing variables to keep and renaming
    select(exp = "Experiment(s)", stat_id = "Station ID", month = "Month", year = "Year", temp = "Calibrated Temperature [C]", dew = "Calibrated Dew Point [C]", humid = "Calibrated Relative Humidity [%]", solar = "Solar Radiation [W/m2]", rain = "Rainfall [mm]", wind_spd = "Calibrated Wind Speed [m/s]", wind_dir = "Calibrated Wind Direction [degrees]", wind_gust = "Calibrated Wind Gust [m/s]", soil_temp = "Soil Temperature [C]", soil_moist = "Soil Moisture [%]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(exp, stat_id, year, month) %>% 
    
    # changing the sort    
    arrange(exp, stat_id, year, month ) %>% 
    # converts the weather variables to numeric
    modify_at(5:15, as.numeric) %>% 
    # creates new variables on with summary statistics and drops all the other variables that weren't grouped
    # so these are the min/max of each day
    summarise_if(is.numeric, funs(min, max, mean, median), na.rm = TRUE) %>% 
    # Split exp with multiple sites
    separate_rows(exp)

# add the meta data
#wthmon <- left_join(wthmon, meta, by = "exp")
colnames(wthday)[1] = 'exp'
wthday <- left_join(wthday, meta, by = "exp")



# tidy the data
hyb %<>% 
    # choosing variables to keep and renaming
    select(exp = "Field-Location", pedi = "Pedigree", repl = "Replicate", planted = "Date Planted", harvested = "Date Harvested", plant_ht = "Plant height [cm]", ear_ht = "Ear height [cm]",test_wt = "Test weight [lbs]", plot_wt = "Plot Weight [lbs]", yield = "Grain yield [bu/acre]") %>% 
    # changing the sort 
    arrange(exp, pedi, repl) %>% drop_na(yield)

##### Month #####
# joining the tidy weather data with min/max variables
# left join to preserve hybrid data and fill matching weather data to each expermient

#hybmon <- left_join(hyb, wthmon, by = "exp") %>%  
#    select(1:3, 11:13, 54:56, 4:10, 14:53) %>% 
#    drop_na(17:56)

hybday <- left_join(hyb, wthday, by = "exp") %>%  
    select(1:3, 11:13, 54:56, 4:10, 14:53) %>% 
    drop_na(17:56)

# check for NA's

# missing <- hybmon %>% 
#     select_if(function(x) any(is.na(x))) %>% 
#     summarise_all(funs(sum(is.na(.))))

missing <- hybday %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

#write_rds(hybmon, "~/EnviroTyping/data/interim/2015/hyb_by_mon_calib.rds")


write_rds(hybday, "~/EnviroTyping/data/interim/2015/hyb_by_day_calib.rds", compress = "xz")

# Building a second data set that allows missing weather variables
# hybmon_with_missing <- left_join(hyb, wthmon, by = "exp") %>% 
#     select(1:3, 11:13, 54:56, 4:10, 14:53)

hybday_with_missing <- left_join(hyb, wthday, by = "exp") %>% 
    select(1:3, 11:13, 54:56, 4:10, 14:53)


# check for NA's
# missing_true <- hybmon_with_missing %>% 
#     select_if(function(x) any(is.na(x))) %>% 
#     summarise_all(funs(sum(is.na(.))))

missing_true <- hybday_with_missing %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

# write_rds(hybmon_with_missing, "~/EnviroTyping/data/interim/2015/hyb_by_mon_calib_w_wth_nas.rds", compress = "xz")

write_rds(hybday_with_missing, "~/EnviroTyping/data/interim/2015/hyb_by_day_calib_w_wth_nas.rds", compress = "xz")

# write_csv(missing_true, "~/EnviroTyping/data/interim/2015/missing_wth_counts.csv")

write_csv(missing_true, "~/EnviroTyping/data/interim/2015/missing_wth_counts_day.csv")




###########################################
############################################
##############################################
#############################################
############################################

library(magrittr)

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/a._2015_hybrid_phenotypic_data/g2f_2015_hybrid_data_no_outliers.csv",col_types = cols("Date Planted" = col_date("%m/%d/%Y"), "Date Harvested" = col_date("%m/%d/%Y"), "Plant height [cm]" = col_number(), "Ear height [cm]" = col_number()))

wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/c._2015_weather_data/g2f_2015_weather_calibrated.csv")

meta <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/z._2015_supplemental_info/g2f_2015_field_metadata.csv") %>% select(exp = "Experiment", city = "City", lat = "WS Lat", lon = "WS Lon")

##### Week #####


#every time Day goes up by 7 tick up Week by 1

Date = NULL

Date = as.Date(with(wth, paste(2015, Month, Day, sep = '-')), "%Y-%m-%d")

Week = as.numeric(strftime(Date, format = "%V"))

wth$Month = Week
colnames(wth)[7] = "Week"
colnames(wth)[7]
# tidy the data
wthwk <- wth %>% 
    # choosing variables to keep and renaming
    select(exp = "Experiment(s)", stat_id = "Station ID", wk = "Week", year = "Year", temp = "Calibrated Temperature [C]", dew = "Calibrated Dew Point [C]", humid = "Calibrated Relative Humidity [%]", solar = "Solar Radiation [W/m2]", rain = "Rainfall [mm]", wind_spd = "Calibrated Wind Speed [m/s]", wind_dir = "Calibrated Wind Direction [degrees]", wind_gust = "Calibrated Wind Gust [m/s]", soil_temp = "Soil Temperature [C]", soil_moist = "Soil Moisture [%]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(exp, stat_id, year, wk) %>% 
    
    # changing the sort    
    arrange(exp, stat_id, year, wk ) %>% 
    # converts the weather variables to numeric
    modify_at(5:15, as.numeric) %>% 
    # creates new variables on with summary statistics and drops all the other variables that weren't grouped
    # so these are the min/max of each day
    summarise_if(is.numeric, funs(min, max, mean, median), na.rm = TRUE) %>% 
    # Split exp with multiple sites
    separate_rows(exp)

# add the meta data
wthwk <- left_join(wthwk, meta, by = "exp")
#colnames(wthday)[1] = 'exp'
#wthday <- left_join(wthday, meta, by = "exp")



# tidy the data
hyb %<>% 
    # choosing variables to keep and renaming
    select(exp = "Field-Location", pedi = "Pedigree", repl = "Replicate", planted = "Date Planted", harvested = "Date Harvested", plant_ht = "Plant height [cm]", ear_ht = "Ear height [cm]",test_wt = "Test weight [lbs]", plot_wt = "Plot Weight [lbs]", yield = "Grain yield [bu/acre]") %>% 
    # changing the sort 
    arrange(exp, pedi, repl) %>% drop_na(yield)

##### Month #####
# joining the tidy weather data with min/max variables
# left join to preserve hybrid data and fill matching weather data to each expermient

hybwk <- left_join(hyb, wthwk, by = "exp") %>%  
    select(1:3, 11:13, 54:56, 4:10, 14:53) %>% 
    drop_na(17:56)

hybday <- left_join(hyb, wthday, by = "exp") %>%  
    select(1:3, 11:13, 54:56, 4:10, 14:53) %>% 
    drop_na(17:56)

# check for NA's

missing <- hybwk %>% 
     select_if(function(x) any(is.na(x))) %>% 
     summarise_all(funs(sum(is.na(.))))

missing <- hybday %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybwk, "data/interim/2015/hyb_by_wk_calib.rds")


write_rds(hybday, "~/EnviroTyping/data/interim/2015/hyb_by_day_calib.rds", compress = "xz")

 Building a second data set that allows missing weather variables
hybwk_with_missing <- left_join(hyb, wthwk, by = "exp") %>% 
     select(1:3, 11:13, 54:56, 4:10, 14:53)

hybday_with_missing <- left_join(hyb, wthday, by = "exp") %>% 
    select(1:3, 11:13, 54:56, 4:10, 14:53)


# check for NA's
missing_true <- hybwk_with_missing %>% 
     select_if(function(x) any(is.na(x))) %>% 
     summarise_all(funs(sum(is.na(.))))

missing_true <- hybday_with_missing %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybwk_with_missing, "data/interim/2015/hyb_by_wk_calib_w_wth_nas.rds", compress = "xz")

write_rds(hybday_with_missing, "~/EnviroTyping/data/interim/2015/hyb_by_day_calib_w_wth_nas.rds", compress = "xz")

# write_csv(missing_true, "~/EnviroTyping/data/interim/2015/missing_wth_counts.csv")

write_csv(missing_true, "~/EnviroTyping/data/interim/2015/missing_wth_counts_day.csv")
