# imports all the packages i'll need
library(tidyverse)

setwd("~/Stapleton_Lab/Projects/Premium/weatherAnalysis/")

# import cleaned weather dataset
wth <- read_csv("~/Stapleton_Lab/Downloads/g2f_2015_weather_clean.csv")

# tidy the data
wth1 <- wth %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Day, Month, Year, Temp = "Temperature [C]",
            Dew = "Dew Point [C]", Humid = "Relative Humidity [%]", Solar = "Solar Radiation [W/m2]", 
            Rain = "Rainfall [mm]", windSpd = "Wind Speed [m/s]", windDir = "Wind Direction [degrees]", 
            windGust = "Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Year, Month) %>% 

    # changing the sort    
    arrange(Exp, StatID, Year, Month ) %>% 
    
    # removes NA's
    drop_na()

# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wth2 <- wth1 %>% 
    summarise(tempMin = min(Temp), tempMax = max(Temp), dewMin = min(Dew), dewMax = max(Dew), 
              humidMin = min(Humid), humidMax = max(Humid), solarMin = min(Solar), solarMax = max(Solar),
              rainMin = min(Rain), rainMax = max(Rain), windSpdMin = min(windSpd), windSpdMax = max(windSpd),
              windDirMin = min(windDir), windDirMax = max(windDir), windGustMin = min(windGust),
              windGustMax = max(windGust), soilTempMin = min(soilTemp), soilTempMax = max(soilTemp),
              soilMoistMin = min(soilMoist), soilMoistMax = max(soilMoist))



