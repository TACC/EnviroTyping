library(magrittr)

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Nov_2016_V.3/a._2014_hybrid_phenotypic_data/g2f_2014_hybrid_no_outliers.csv" ,col_types = cols("Date Planted" = col_date("%m/%d/%Y"), "Date Harvested" = col_date("%m/%d/%Y"), "Plant height [cm]" = col_number(), "Ear height [cm]" = col_number()))

wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Nov_2016_V.3/c._2014_weather_data/g2f_2014_weather_calibrated.csv")

meta <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Nov_2016_V.3/z._2014_supplemental_info/g2f_2014_field_characteristics.csv") %>% 
    select(exp = "Experiment", city = "City",lat, lon = "long")

##### Month #####
# tidy the data
wthmon <- wth %>% 
    # choosing variables to keep and renaming
    select(exp = "Experiment(s)", stat_id = "Station ID", month = "Month [Local]", year = "Year [Local]", temp = "Calibrated Temperature [C]", dew = "Calibrated Dew Point [C]", humid = "Calibrated Relative Humidity [%]", solar = "Solar Radiation [W/m2]", rain = "Rainfall [mm]", wind_spd = "Calibrated Wind Speed [m/s]", wind_dir = "Calibrated Wind Direction [degrees]", wind_gust = "Calibrated Wind Gust [m/s]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(exp, stat_id, year, month) %>% 
    # changing the sort    
    arrange(exp, stat_id, year, month ) 

# converts the weather variables to numeric
wthmon %<>% modify_at(5:12, as.numeric)


# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wthmon %<>% summarise_if(is.numeric, funs(min, max, mean, median), na.rm = TRUE)

# Split exp with multiple sites

wthmon %<>% separate_rows(exp)

wthmon <- left_join(wthmon, meta, by = "exp")

# tidy the data
hyb %<>% 
    # choosing variables to keep and renaming
    select(exp = "Field-Location", pedi = "Pedigree", rep = "Rep", planted = "Date Planted", harvested = "Date Harvested", plant_ht = "Plant height [cm]", ear_ht = "Ear height [cm]",test_wt = "Test Weight [lbs/bu]", plot_wt = "Plot Weight [lbs]", yield = "Grain yield [bu/A]") %>% 
    
    # changing the sort 
    arrange(exp, pedi, rep)

##### Month #####
# joining the tidy weather data with min/max variables
# left join to preserve hybrid data and fill matching weather data to each expermient
hybmon <- left_join(hyb, wthmon, by = "exp") %>% 
    select(1,11,2:3,46:48,12:13,4:10,14:45) %>% 
    drop_na(16:48)

# check for NA's
missing <- hybmon %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybmon, "~/github/EnviroTyping/data/interim/2014/hyb_by_mon_calib.rds", compress = "xz")

hybmon_with_missing <- left_join(hyb, wthmon, by = "exp") %>% 
    select(1,11,2:3,46:48,12:13,4:10,14:45)

# check for NA's
missing_true <- hybmon_with_missing %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))


write_rds(hybmon_with_missing, "~/github/EnviroTyping/data/interim/2014/hyb_by_mon_calib_w_wth_nas.rds", compress = "xz")
write_csv(missing_true, "~/github/EnviroTyping/data/interim/2014/missing_wth_counts.csv")

extra.repl <- hybmon %>% group_by(Exp, Pedi, Repl) %>% summarise(count = n()) %>% filter(count > 5)
write_csv(extra.repl, "../../interim_datasets/2016/extra_repl.csv")
