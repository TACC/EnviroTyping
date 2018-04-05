library(PReMiuM)
library(tidyverse)
library(profvis)
library(lubridate)
library(tictoc)
setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k")

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k/")

# read in data from analysis script
hybridWeek <- read_rds("../hybrid_by_week_cleaned_weather.Rds")

# creates a data frame for the month number
temp <- hybridWeek %>% filter(Week >= isoweek(Planted) + 3 & Week <= isoweek(Planted) + 5 )
unique(temp$Week)
# find continous variables with variance
val <- grep("Min|Max",names(temp))

tic(); numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))];toc() # 0.006 sec elapsed
tic(); nv <- names(which(map_dbl(temp[val], var) != 0));toc() # 0.004 sec elapsed
tic(); nv1 <- names(which(map_lgl(temp[val],~ var(.) != 0))); toc() # 0.005 sec elapsed

setwd("./output")

p <- profvis({
    runInfoObj <- profRegr(covNames, outcome = 'Yield',
                    yModel = 'Normal', xModel = "Mixed",
                    discreteCovs = "Pedi",
                    continuousCovs = numericVars,
                    data = temp,
                    nSweeps = 1000,
                    nBurn = 1000)
    
    
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    
    clusObj <- calcOptimalClustering(calcDists)
    
    riskProfObj <- calcAvgRiskAndProfile(clusObj)
    
    saveRDS(riskProfObj, file = "riskProfObj.rda")
    
    plotRiskProfile(riskProfObj,  whichCovariates = c("tempMin", "tempMax", "dewMin", "dewMax", "humidMin", "humidMax", "solarMin", "solarMax", "rainMax"), outFile = "temp_dew_humid_solar_rainMax.png")
    
    plotRiskProfile(riskProfObj,  whichCovariates = c("windSpdMin", "windSpdMax", "windDirMin", "windDirMax", "windGustMin", "windGustMax", "soilTempMin", "soilTempMax", "soilMoistMin", "soilMoistMax"), outFile = "windSpd_windDir_windGust_soilTemp_soilMoist.png")

    })
htmlwidgets::saveWidget(p, "profile.html")
