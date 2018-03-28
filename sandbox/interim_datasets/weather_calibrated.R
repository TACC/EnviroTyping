# imports all the packages i'll need
library(tidyverse)
library(lubridate)

setwd("~/GitHub/EnviroTyping/")
# import cleaned weather dataset
wth <- read_csv("data/external/G2F from Cyverse DataStore/g2f_2015_weather_calibrated.csv")

meta <- read_csv("data/external/G2F from Cyverse DataStore/g2f_2015_field_metadata.csv")

meta <- meta %>% select(Exp = "Experiment", City, Lat = "WS Lat", Lon = "WS Lon")

##### Day #####
# tidy the data
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

# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
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


# Split Exp with multiple sites
wthday <- wthday %>% 
    ungroup(Exp) %>% 
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>% 
    unnest(Exp) %>% 
    select(Exp, everything())

wthday$Exp[wday$Exp == ""] <- "NA"

wthday <- wthday %>% filter(Exp != "NA") 

wthday <- inner_join(wthday, meta, by = "Exp")

##### Week #####
# tidy the data
wthwk <- wth %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Day, Month, Year, DoY = "Day of Year", Temp = "Calibrated Temperature [C]",
           Dew = "Calibrated Dew Point [C]", Humid = "Calibrated Relative Humidity [%]", Solar = "Solar Radiation [W/m2]", 
           Rain = "Rainfall [mm]", windSpd = "Calibrated Wind Speed [m/s]", windDir = "Calibrated Wind Direction [degrees]", 
           windGust = "Calibrated Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%]") %>% 
    
    # get the date as one column
    mutate(Date = make_date(Year, Month, Day)) %>%
    
    # get the week of the year
    mutate(Week = isoweek(Date)) %>% 
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Month, Week) %>% 
    
    # changing the sort    
    arrange(Exp, StatID, Month, Week) %>% 
    
    # removes NA's
    drop_na()

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
    select(Exp, everything())

wthwk$Exp[wthwk$Exp == ""] <- "NA"

wthwk <- wthwk %>% filter(Exp != "NA")

wthwk <- inner_join(wthwk, meta, by = "Exp")

##### Month #####
# tidy the data
wthmon <- wth %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Day, Month, Year, DoY = "Day of Year", Temp = "Calibrated Temperature [C]",
           Dew = "Calibrated Dew Point [C]", Humid = "Calibrated Relative Humidity [%]", Solar = "Solar Radiation [W/m2]", 
           Rain = "Rainfall [mm]", windSpd = "Calibrated Wind Speed [m/s]", windDir = "Calibrated Wind Direction [degrees]", 
           windGust = "Calibrated Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%]") %>% 
    
    # grouping by variables for making summary statistics
    group_by(Exp, StatID, Year, Month) %>% 
    
    # changing the sort    
    arrange(Exp, StatID, Year, Month ) %>% 
    
    # removes NA's
    drop_na()

# creates new variables on with summary statistics and drops all the other variables that weren't grouped
# so these are the min/max of each day
wthmon <- wthmon %>% 
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
wthmon <- wthmon %>% 
    ungroup(Exp) %>% 
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>% 
    unnest(Exp) %>% 
    select(Exp, everything()) %>% 
    drop_na(Exp)

wthmon$Exp[wthmon$Exp == ""] <- "NA"

wthmon <- wthmon %>% filter(Exp != "NA")

wthmon <- inner_join(wthmon, meta, by = "Exp")
