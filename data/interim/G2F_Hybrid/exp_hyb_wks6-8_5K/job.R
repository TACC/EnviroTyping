library(PReMiuM)
library(dplyr)
library(readr)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/exp_hyb_wks6-8_5K")

# read in data from analysis script
df <- read_csv("./data.csv",col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number()))

# creates a data frame for the month number
temp <- df %>% filter(seed_6_8 == TRUE)

# find continous variables with variance
val <- grep("Min|Max",names(temp))

numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]


setwd("./output")

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                yModel = 'Normal', xModel = "Mixed",
                discreteCovs = c("Exp", "Pedi"),
                continuousCovs = numericVars,
                data = temp,
                nSweeps = 5000,
                nBurn = 5000)


calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

riskProfObj <- calcAvgRiskAndProfile(clusObj)

saveRDS(riskProfObj, file = "riskProfObj.rda")

clusterOrderObj<-plotRiskProfile(riskProfileOb,  whichCovariates = c("Exp", "tempMin", "tempMax", "dewMin", "dewMax", "humidMin", "humidMax", "solarMin", "solarMax", "rainMax"), outFile = "temp_dew_humid_solar_rainMax.png")

clusterOrderObj<-plotRiskProfile(riskProfileOb,  whichCovariates = c("windSpdMin", "windSpdMax", "windDirMin", "windDirMax", "windGustMin", "windGustMax", "soilTempMin", "soilTempMax", "soilMoistMin", "soilMoistMax"), outFile = "windSpd_windDir_windGust_soilTemp_soilMoist.png")

