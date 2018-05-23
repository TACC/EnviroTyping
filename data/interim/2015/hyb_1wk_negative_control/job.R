library(PReMiuM)
library(tidyverse)
library(lubridate)
library(tictoc)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_1wk_negative_control")

# read in data from analysis script
df <- read_rds("../hybrid_by_week_cleaned_weather.Rds")

# creates a data frame for the month number
temp <- df %>% filter(Week == isoweek(Planted))
unique(isoweek(temp$Planted))
unique(temp$Week)
table(temp$Exp)
length(unique(temp$Pedi))

# find continous variables with variance
val <- grep("Min|Max",names(temp))

numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]


setwd("./output")


runInfoObj <- profRegr(covNames, outcome = 'Yield',
                yModel = 'Normal', xModel = "Mixed",
                discreteCovs = "Pedi",
                continuousCovs = numericVars,
                data = temp,
                nSweeps = 1000,
                nBurn = 1000)


calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)
tic()
riskProfObj <- calcAvgRiskAndProfile(clusObj)
toc()
write_rds(riskProfObj, path = "../riskProfObj.rds")

plotRiskProfile(riskProfObj,  whichCovariates = c("tempMin", "tempMax", "dewMin", "dewMax", "humidMin", "humidMax", "solarMax", "rainMax"), outFile = "temp_dew_humid_solar_rainMax.png")

plotRiskProfile(riskProfObj,  whichCovariates = c("windSpdMin", "windSpdMax", "windDirMin", "windDirMax", "windGustMin", "windGustMax", "soilTempMin", "soilTempMax", "soilMoistMin", "soilMoistMax"), outFile = "windSpd_windDir_windGust_soilTemp_soilMoist.png")

riskProfObj <- read_rds("../riskProfObj.rds")


