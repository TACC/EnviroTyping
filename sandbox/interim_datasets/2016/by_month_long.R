library(magrittr)

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_G2F_2016_Data_Mar_2018/a._2016_hybrid_phenotypic_data/g2f_2016_hybrid_data_no_outliers.csv",col_types = cols("Date Plot Planted" = col_date("%m/%d/%Y"), "Date Plot Harvested" = col_date("%m/%d/%Y"), "Plant Height [cm]" = col_number(), "Ear Height [cm]" = col_number()))

wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_G2F_2016_Data_Mar_2018/c._2016_weather_data/g2f_2016_weather_calibrated.csv")

meta <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_G2F_2016_Data_Mar_2018/z._2016_supplemental_info/g2f_2016_field_metadata.csv") %>% 
    select(exp = "Experiment Code", city = "City", lat = "Weather station latitude (in decimal numbers NOT DMS)", lon = "Weather station longitude (in decimal numbers NOT DMS)")

##### Month #####
# tidy the data
wthmon <- wth %>% 
    # choosing variables to keep and renaming
    select(exp = "Experiment(s)", stat_id = "Station ID", month = "Month", year = "Year", temp = "Calibrated Temperature [C]", dew = "Calibrated Dew Point [C]", humid = "Calibrated Relative Humidity [%]", solar = "Solar Radiation [W/m2]", rain = "Rainfall [mm]", wind_spd = "Calibrated Wind Speed [m/s]", wind_dir = "Calibrated Wind Direction [degrees]", wind_gust = "Calibrated Wind Gust [m/s]", soil_temp = "Soil Temperature [C]", soil_moist = "Soil Moisture [%VWC]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(exp, stat_id, year, month) %>% 
    
    # changing the sort    
    arrange(exp, stat_id, year, month )

# converts the weather variables to numeric
wthmon %<>% modify_at(5:14, as.numeric)


# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wthmon %<>% summarise_if(is.numeric, funs(min, max, mean, median), na.rm = TRUE)

# Split Exp with multiple sites
wthmon %<>% separate_rows(exp)

wthmon <- left_join(wthmon, meta, by = "exp")

# tidy the data
hyb <- hyb %>% 
    # choosing variables to keep and renaming
    select(exp = "Field-Location", pedi = "Pedigree", rep = "Replicate", planted = "Date Plot Planted", harvested = "Date Plot Harvested", plant_ht = "Plant Height [cm]", ear_ht = "Ear Height [cm]",test_wt = "Test Weight [lbs/bu]", plot_wt = "Plot Weight [lbs]", yield = "Grain Yield [bu/acre]") %>% 
    
    # changing the sort 
    arrange(exp, pedi, rep)

##### Month #####
# joining the tidy weather data with min/max variables
# left join to preserve hybrid data and fill matching weather data to each expermient
hybmon <- left_join(hyb, wthmon, by = "exp") %>%  
    select(1:5, 11:13, 54:56, 6:10, 14:53) %>% 
    drop_na(16:56)

# check for NA's
missing <- hybmon %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybmon, "~/github/EnviroTyping/data/interim/2016/hyb_by_mon_calibr.rds", compress = "xz")

hybmon_with_missing <- left_join(hyb, wthmon, by = "exp") %>%  
    select(1:5, 11:13, 54:56, 6:10, 14:53)

# check for NA's
missing_true <- hybmon_with_missing %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybmon_with_missing, "~/github/EnviroTyping/data/interim/2016/hyb_by_mon_calibr_w_wth_nas.rds", compress = "xz")

write_csv(missing_true, "~/github/EnviroTyping/data/interim/2016/missing_wth_counts.csv")

extra.repl <- hybmon %>% group_by(Exp, Pedi, Repl) %>% summarise(count = n()) %>% filter(count > 5)
write_csv(extra.repl, "../../interim_datasets/2016/extra_repl.csv")
