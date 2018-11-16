To analyze the G2F data first we need to grab it from the [CyVerse DataStore](http://datacommons.cyverse.org/browse/iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017).
### Importing Data
```r
# Install the following packages if you don't already have them
library(tidyverse)
library(lubridate)
# Weather Data
wth <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/c._2015_weather_data/g2f_2015_weather_calibrated.csv")

# Meta Data
meta <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/z._2015_supplemental_info/g2f_2015_field_metadata.csv")

# Hybrid Data

hyb <- read_csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/Carolyn_Lawrence_Dill_G2F_Mar_2017/a._2015_hybrid_phenotypic_data/g2f_2015_hybrid_data_no_outliers.csv")
```
### Cleaning the Data
#### Meta

The latitude and longitude for each field-location need to be extracted from the meta data.
```r
meta <- meta %>% select(Exp = "Experiment", City, Lat = "WS Lat", Lon = "WS Lon")
```
#### Weather
First, the weather data needs to be summarized by Day, Week, and Month.  The original data subset at 30 minute intervals.

##### Day
First step is to tidy the data.
```r
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
```
Next create new variables with summary statistics and drops all the other variables that weren't grouped.  The result is the minimum, maximum, median, and mean for each of the weather variables
```r
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
```

The variable **Experiment** has nested locations which need to be split apart.
```r
wthday <- wthday %>%
    ungroup(Exp) %>%
    mutate(Exp = strsplit(as.character(Exp), " ") ) %>%
    unnest(Exp) %>%
    select(Exp, everything())

wthday$Exp[wthday$Exp == ""] <- "NA"

wthday <- wthday %>% filter(Exp != "NA")
```
The latitude and longitude are merged to the weather date.
```r
wthday <- inner_join(wthday, meta, by = "Exp")
```
The same workflow is applied to Week and Month subsets.
##### Week
```r
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

# creates new variables with summary statistics and drops all the other variables that weren't grouped
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

# Merge meta and weather data
wthwk <- inner_join(wthwk, meta, by = "Exp")
```
##### Month
```r
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

# creates new variables with summary statistics and drops all the other variables that weren't grouped
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

# Merge meta and weather data
wthmon <- inner_join(wthmon, meta, by = "Exp")
```
#### Hybrid
Just like with the weather data the hybrid data needs to be tidied.
```r
hyb <- hyb %>%

    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Planted = "Date Planted",Harvest = "Date Harvested",plantHt = "Plant height [cm]", earHt = "Ear height [cm]",testWt = "Test weight [lbs]",plotWt = "Plot Weight [lbs]", Yield = "Grain yield [bu/acre]") %>%

    # changing the sort
    arrange(Exp, Pedi, Repl)
```
Then each of the weather by time subset dataframes are merged to the hybrid data by the **Experiment** variable.  The data is checked for any missing values in the yield or weather variables.  If everything looks okay is it written to both a CSV and Rds file for repeated use.
```r
# Day
hybday <- right_join(hyb, wthday, by = "Exp") %>%  
    drop_na(Yield) %>%
    select(1:5, 11:12, 53:55, 6:10, 13:52)

hybday %>%
    select_if(function(x) any(is.na(x))) %>%
    summarise_all(funs(sum(is.na(.))))

write_csv(hybday, "data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.csv")
write_rds(hybday, "data/interim/G2F_Hybrid/hybrid_by_day_calibrated_weather.rds", compress = "xz")

# Week
hybwk <- right_join(hyb, wthwk, by = "Exp") %>%  
    drop_na(Yield) %>%
    select(1:5, 11:13, 54:56, 6:10, 14:53)

hybwk %>%
    select_if(function(x) any(is.na(x))) %>%
    summarise_all(funs(sum(is.na(.))))


write_csv(hybwk, "data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.csv")
write_rds(hybwk, "data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.rds", compress = "xz")

# Month #
hybmon <- right_join(hyb, wthmon, by = "Exp") %>%  
    drop_na(Yield) %>%
    select(1:5, 11:13, 54:56, 6:10, 14:53)

hybmon %>%
    select_if(function(x) any(is.na(x))) %>%
    summarise_all(funs(sum(is.na(.))))

write_csv(hybmon, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.csv")
write_rds(hybmon, "data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather.rds", compress = "xz")
```
To verify the datasets were written correctly they are read from storage and compared as R objects.
```r
checkr <- read_rds("data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.rds")
checkc <- read_csv("data/interim/G2F_Hybrid/hybrid_by_week_calibrated_weather.csv")
identical(hybwk, checkr)
```
