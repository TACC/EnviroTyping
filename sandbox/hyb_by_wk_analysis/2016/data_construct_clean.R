library(tidyverse)
library(lubridate)
library(magrittr)

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_G2F_2016_Data_Mar_2018/a._2016_hybrid_phenotypic_data/g2f_2016_hybrid_data_no_outliers.csv",col_types = cols("Date Plot Planted" = col_date("%m/%d/%Y"), "Date Plot Harvested" = col_date("%m/%d/%Y"), "Plant Height [cm]" = col_number(), "Ear Height [cm]" = col_number()))

wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_G2F_2016_Data_Mar_2018/c._2016_weather_data/g2f_2016_weather_clean.csv")

meta <- read_csv("data/external/G2F/g2f_2016_field_metadata.csv")

meta <- meta %>% select(Exp = "Experiment Code", City, Lat = "Weather station latitude (in decimal numbers NOT DMS)", Lon = "Weather station longitude (in decimal numbers NOT DMS)")


Date = NULL

Date = as.Date(with(wth, paste(2016, Month, Day, sep = '-')), "%Y-%m-%d")

Week = as.numeric(strftime(Date, format = "%V"))

wth$Month = Week
colnames(wth)[5] = "Week"
colnames(wth)[5]
unique(wth$Week)

##### Week #####
# tidy the data
wthwk <- wth %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Day, Week, Year, DoY = "Day of Year", Temp = "Temperature [C]",
           Dew = "Dew Point [C]", Humid = "Relative Humidity [%]", Solar = "Solar Radiation [W/m2]", 
           Rain = "Rainfall [mm]", windSpd = "Wind Speed [m/s]", windDir = "Wind Direction [degrees]", 
           windGust = "Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%VWC]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Year, Week) %>% 
    
    # changing the sort    
    arrange(Exp, StatID, Year, Week) %>% 
    
    # removes NA's
    drop_na()


cols <- names(wthwk)[7:16]
wthwk %<>% mutate_at(cols,funs(as.numeric(.)))
# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wthwk <- wthwk %>% 
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

# Split Exp with multiple sites
wthwk <- wthwk %>% 
    ungroup(Exp) %>% 
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>% 
    unnest(Exp) %>% 
    select(Exp, everything()) %>% 
    drop_na(Exp)

wthwk$Exp[wthwk$Exp == ""] <- "NA"

wthwk <- wthwk %>% filter(Exp != "NA")

wthwk <- inner_join(wthwk, meta, by = "Exp")

# tidy the data
hyb <- hyb %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Planted = "Date Plot Planted",Harvest = "Date Plot Harvested",plantHt = "Plant Height [cm]", earHt = "Ear Height [cm]",testWt = "Test Weight [lbs/bu]",plotWt = "Plot Weight [lbs]", Yield = "Grain Yield [bu/acre]") %>% 
    
    # changing the sort 
    arrange(Exp, Pedi, Repl)

##### Month #####
# joining the tidy weather data with min/max variables
# right join to preserve weather data and fill matching hybrid data to each expermient
hybwk <- right_join(hyb, wthwk, by = "Exp") %>%  
    drop_na(Yield) %>% 
    select(1:5, 11:13, 54:56, 6:10, 14:53)

# check for NA's
hybwk %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

write_rds(hybwk, "data/interim/2016/hyb_by_wk_calib_clean.rds")


###Wide

library(tidyverse)
df <- read_rds("data/interim/2016/hyb_by_wk_calib_clean.rds")
val <- str_which(names(df),"Min|Max|Mean|Median")
numericVars <- names(which(map_dbl(df[,17:56], var, na.rm = TRUE) != 0))
df %>% select(1:16,numericVars)
df1 <- df %>% 
    gather(Var,val,17:56) %>% 
    unite(Var1, Var, Week) %>% 
    spread(Var1, val)
str(df1)

write_rds(df1, "data/interim/2016/hyb_by_wk_calib_clean_wide.rds", compress = "xz")






