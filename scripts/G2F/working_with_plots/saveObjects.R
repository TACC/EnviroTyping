library(PReMiuM)
library(dplyr)
library(readr)
library(ggplot2)
library(grid)
library(lineprof)

df <- read_csv("data/interim/G2F_Hybrid/hybrid_bymonth_cleaned_weather.csv",
            col_types = cols( humidMin = col_double(),
                              solarMin = col_double(),
                              solarMax = col_double(),
                              rainMin = col_double(),
                              rainMedian = col_double(),
                              windDirMin = col_double(),
                              windDirMax = col_double()))

setwd("scripts/G2F/working_with_plots")
temp <- df %>% filter(Month==7)
str(temp)
val <- grep("Min|Max",names(temp))
# numericVars <- numericVars <- names(sort(unlist(lapply(temp[val],function(x) var(x))), decreasing = TRUE)[1:14])
numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]
setwd("output")

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                yModel = 'Normal', xModel = "Mixed",
                discreteCovs = "Pedi",
                continuousCovs = numericVars[1:14],
                data = temp,
                nSweeps = 100,
                nBurn = 100)


calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

riskProfObj <- calcAvgRiskAndProfile(clusObj)

# saveRDS(riskProfObj, "../riskProfObj.rda")

# whichCovariates <-  riskProfObj$riskProfClusObj$clusObjRunInfoObj$covNames[1:15]

clusterOrderObj <- plotRiskProfile(riskProfObj,"summary.png")

