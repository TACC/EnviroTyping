library(tidyverse)
library(lubridate)
library(viridis)

setwd("~/GitHub/EnviroTyping/")
# import cleaned weather dataset
wth <- read_csv("data/external/G2F/g2f_2015_weather_calibrated.csv")

meta <- read_csv("data/external/G2F/g2f_2015_field_metadata.csv")

meta <- meta %>% select(Exp = "Experiment", City, Lat = "WS Lat", Lon = "WS Lon")

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

hybrid_loations <- str_subset(unique(wthmon$Exp), "H")
median_vars <- str_subset(names(wthmon), "Median")

hybrids <- wthmon %>% filter(Exp %in% hybrid_loations) %>% select(Exp, Month, median_vars)
gather(hybridByWeekSubset,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)))

p <- ggplot(gather(hybrids, key, value, -c(Exp,Month)), aes(x = Month, y = value)) + 
    geom_point(aes(color = Exp)) +
    geom_line(aes(color = Exp)) + 
    facet_wrap(~ key, scales="free_y", ncol = 5) + 
    theme_bw() #scale_color_viridis(discrete = TRUE)
    
p

library(RColorBrewer)
display.brewer.all()

l <- list()
t <- list()
for (i in 1:14){
    temp <- weeks3_5Groups%>% filter(group==i)
    temp <- apply(temp[5:24],2,summary)
    t[[i]] <- temp
    data <- rowid_to_column(as.tibble(t(scale(temp))), "ID")
    data <- data %>% select(ID, Min = Min., Median, Max = Max.)
    df <- melt(data ,  id.vars = 'ID', variable.name = 'series')
    
    p <- ggplot(df, aes(ID,value)) + geom_point(aes(color=series, size = 10), show.legend = FALSE) + labs(title = paste("Scaled Weather Profile for Group",i,sep = " ")) +
        scale_x_continuous(breaks = c(1:19), labels = names(weeks3_5Groups[,c(6:13,15:25)])) + theme_bw() +
        theme(axis.text.x = element_text(face="bold", size=6, angle=45,margin = margin(t = 10)), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank())
    l[[i]] <- p
}

#pdf("profileByGroup.pdf", paper = "a4r")
grid.arrange(l[[1]],l[[2]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]], ncol = 2)
#dev.off()