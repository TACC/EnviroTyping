library(lubridate)
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
    separate_rows(exp) %>% 
    # add the meta data
    left_join(wthmon, meta, by = "exp")

# tidy the data
hyb %<>% 
    # choosing variables to keep and renaming
    select(exp = "Field-Location", pedi = "Pedigree", repl = "Replicate", planted = "Date Planted", harvested = "Date Harvested", plant_ht = "Plant height [cm]", ear_ht = "Ear height [cm]",test_wt = "Test weight [lbs]", plot_wt = "Plot Weight [lbs]", yield = "Grain yield [bu/acre]") %>% 
    # changing the sort 
    arrange(exp, pedi, repl)

##### Month #####
# joining the tidy weather data with min/max variables
# left join to preserve hybrid data and fill matching weather data to each expermient
hybmon <- left_join(hyb, wthmon, by = "exp") 
%>%  
    select(1:3, 11:13, 54:56, 6:10, 14:53) %>% 
    drop_na(16:56)

# check for NA's
na.s <- hybmon %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybmon, "data/interim/2016/hyb_by_mon_calibr.rds")

extra.repl <- hybmon %>% group_by(Exp, Pedi, Repl) %>% summarise(count = n()) %>% filter(count > 5)
write_csv(extra.repl, "../../interim_datasets/2016/extra_repl.csv")
