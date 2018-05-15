wthmon <- wth %>% 
    # choosing variables to keep and renaming
    select(Exp = "Experiment(s)", StatID = "Station ID", Month, Year, Temp = "Calibrated Temperature [C]", Dew = "Calibrated Dew Point [C]", Humid = "Calibrated Relative Humidity [%]", Solar = "Solar Radiation [W/m2]", Rain = "Rainfall [mm]", windSpd = "Calibrated Wind Speed [m/s]", windDir = "Calibrated Wind Direction [degrees]", windGust = "Calibrated Wind Gust [m/s]", soilTemp = "Soil Temperature [C]", soilMoist = "Soil Moisture [%VWC]")
wthmon <- wthmon %>% group_by(StatID)
na.s <- wthmon %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))
