library(tidyverse)
library(lubridate)
library(magrittr)

hyb <- read_csv("data/external/G2F/g2f_2014_hybrid_no_outliers.csv", col_types = cols("Date Planted" = col_date("%m/%d/%Y"), "Date Harvested" = col_date("%m/%d/%Y")))

wth <- read_csv("data/external/G2F/g2f_2014_weather_calibrated.csv")

meta <- read_csv("data/external/G2F/g2f_2014_field_characteristics.csv")

meta <- meta %>% select(Exp = "Experiment", City, Lat = "lat", Lon = "long")

##### Month #####
# tidy the data
wthmon <- wth %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Day = "Day [Local]", Month = "Month [Local]", Year = "Year [Local]", DoY = "Day of Year [Local]", Temp = "Calibrated Temperature [C]", Dew = "Calibrated Dew Point [C]", Humid = "Calibrated Relative Humidity [%]", Solar = "Solar Radiation [W/m2]",  Rain = "Rainfall [mm]", windSpd = "Calibrated Wind Speed [m/s]", windDir = "Calibrated Wind Direction [degrees]", windGust = "Calibrated Wind Gust [m/s]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Year, Month) %>% 
    
    # changing the sort    
    arrange(Exp, StatID, Year, Month ) %>% 
    
    # removes NA's
    drop_na()


cols <- names(wthmon)[7:14]
wthmon %<>% mutate_at(cols,funs(as.numeric(.)))
# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wthmon <- wthmon %>% 
    summarise(tempMin = min(Temp), tempMax = max(Temp), tempMean = mean(Temp), tempMedian = median(Temp), dewMin = min(Dew), dewMax = max(Dew), dewMean = mean(Dew), dewMedian = median(Dew), humidMin = min(Humid), humidMax = max(Humid), humidMean = mean(Humid), humidMedian = median(Humid), solarMin = min(Solar), solarMax = max(Solar), solarMean = mean(Solar), solarMedian = median(Solar), rainMin = min(Rain), rainMax = max(Rain), rainMean = mean(Rain), rainMedian = median(Rain), windSpdMin = min(windSpd), windSpdMax = max(windSpd), windSpdMean = mean(windSpd), windSpdMedian = median(windSpd), windDirMin = min(windDir), windDirMax = max(windDir), windDirMean = mean(windDir), windDirMedian = median(windDir), windGustMin = min(windGust), windGustMax = max(windGust), windGustMean = mean(windGust), windGustMedian = median(windGust))

# Split Exp with multiple sites
wthmon <- wthmon %>% 
    ungroup(Exp) %>% 
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>% 
    unnest(Exp) %>% 
    select(Exp, everything()) %>% 
    drop_na(Exp)

wthmon$Exp[wthmon$Exp == ""] <- "NA"

wthmon <- wthmon %>% filter(Exp != "NA")

wthmon <- inner_join(wthmon, meta, by = "Exp")

# tidy the data
hyb <- hyb %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Planted = "Date Plot Planted",Harvest = "Date Plot Harvested",plantHt = "Plant Height [cm]", earHt = "Ear Height [cm]",testWt = "Test Weight [lbs/bu]",plotWt = "Plot Weight [lbs]", Yield = "Grain Yield [bu/acre]") %>% 
    
    # changing the sort 
    arrange(Exp, Pedi, Repl)

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

write_rds(hybmon, "data/interim/2016/hyb_by_mon_calibr.rds")



