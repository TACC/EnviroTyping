library(PReMiuM)
library(dplyr)
library(readr)

# read in data from analysis script
df <- read_csv("data/interim/G2F_Hybrid/hybrid_bymonth_cleaned_weather.csv")

temp <- df %>% filter(Month==5)

val <- grep("Min|Max",names(temp))
numericVars <- numericVars <- names(sort(unlist(lapply(temp[val],function(x) var(x))), decreasing = TRUE)[1:14])
dir.create("output")
setwd("output")

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                yModel = 'Normal', xModel = "Mixed",
                discreteCovs = "Pedi",
                continuousCovs = numericVars,
                data = temp,
                nSweeps = 100,
                nBurn = 50)


calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

riskProfObj <- calcAvgRiskAndProfile(clusObj)

clusterOrderObj<-plotRiskProfile(riskProfObj,"summary.png")

saveRDS(riskProfObj, "riskProfObj.rda")

