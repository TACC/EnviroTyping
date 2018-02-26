# imports all the packages i'll need
library(tidyverse)
library(lubridate)

# import cleaned weather dataset
wth <- read_csv("data/external/G2F from Cyverse DataStore/g2f_2015_weather_clean.csv")

# tidy the data
wth1 <- wth %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Day, Month, Year, DoY = "Day of Year", Temp = "Temperature [C]",
           Dew = "Dew Point [C]", Humid = "Relative Humidity [%]", Solar = "Solar Radiation [W/m2]", 
           Rain = "Rainfall [mm]", windSpd = "Wind Speed [m/s]", windDir = "Wind Direction [degrees]", 
           windGust = "Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%]") %>% 
    
    # get the week since the start of the year
    mutate(Date = make_date(Year, Month, Day)) %>% 
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Date) %>% 

    # changing the sort    
    arrange(Exp, StatID, Date) %>% 
    
    # removes NA's
    drop_na()

# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wth2 <- wth1 %>% 
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
wth3 <- wth2 %>% 
    ungroup(Exp) %>% 
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>% 
    unnest(Exp) %>% 
    select(Exp, everything())

wth3$Exp[wth3$Exp == ""] <- "NA"

wth4 <- wth3 %>% filter(Exp != "NA")
