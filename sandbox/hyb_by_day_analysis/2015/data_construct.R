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

