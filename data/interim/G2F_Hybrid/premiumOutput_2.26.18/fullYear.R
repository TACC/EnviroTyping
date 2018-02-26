library(PReMiuM)
library(dplyr)
library(readr)


setwd("/work/04734/dhbrand/stampede2/EnviroTyping/data/interim/G2F_Hybrid/premiumOutput_2.16.18")

# read in data from analysis script
df <- read_csv("../hybrid_bymonth_cleaned_weather.csv", 
               col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(),
                                "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(),
                                "solarMax" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number()))



# find continous covariates with some variance
val <- grep("Min|Max",names(df))

numericVars <- names(df[val])[vapply(df[val], function(x) var(x) != 0, logical(1))]

setwd("./output")

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                yModel = 'Normal', xModel = "Mixed",
                discreteCovs = "Pedi",
                continuousCovs = numericVars,
                data = df,
                nSweeps = 1000,
                nBurn = 1000)

saveRDS(runInfoObj, file = "runInfoObj.rda")

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

riskProfObj <- calcAvgRiskAndProfile(clusObj)

saveRDS(riskProfObj, file = "riskProfObj.rda")

clusterOrderObj<-plotRiskProfile(riskProfObj,  whichCovariates =
                                     c("tempMin", "tempMax", "dewMin","dewMax",                                                        "humidMin", "humidMax", "solarMax", "rainMax"),
                                 outFile = "temp_dew_humid_solar_rain.png")

clusterOrderObj<-plotRiskProfile(riskProfObj,  whichCovariates = c("windSpdMin", "windSpdMax", "windDirMin", "windDirMax",
                                                                     "windGustMin", "windGustMax", "soilTempMin", "soilTempMax",
                                                                     "soilMoistMin", "soilMoistMax"),
                                 outFile = "windSpd_windDir_windGust_soilTemp_soilMoist.png")

