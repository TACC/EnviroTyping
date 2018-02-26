# imports all the packages i'll need
library(tidyverse)

setwd("~/Stapleton_Lab/Premium/EnviroTyping/")

# import cleaned weather dataset
wth <- read_csv("data/external/G2F from Cyverse DataStore/g2f_2015_weather_clean.csv")

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