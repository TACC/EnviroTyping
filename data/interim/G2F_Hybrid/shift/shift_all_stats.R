library(tidyverse)
library(PReMiuM)
library(tictoc)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/shift/output1")

df <- read_rds("../../hybrid_by_month_shift_all_stats.rds")

contVars <- names(df)[grep("Mean",names(df))]

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 10, nBurn = 4,
                       nProgress = 5, seed = 1234, 
                       reportBurnIn = TRUE, nClusInit = 1000)

globalParsTrace(runInfoObj, parameters = "nClusters", plotBurnIn = TRUE)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
dew <- grep("dew",contVars, value = TRUE)
plotRiskProfile(riskProfObj, "summary.png", whichCovariates = dew)

clustering <- riskProfObj$riskProfClusObj$clustering
df1 <- cbind(df, clustering)

df1 %>% select(1:4, clustering) %>% filter(clustering == 2)
df2 <- df %>% filter( !(Pedi == "WF9/H95" & StatID == 8657))

contVars <- names(df2)[grep("Mean",names(df2))]

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df2, nSweeps = 10, nBurn = 4,
                       nProgress = 5, seed = 1234, 
                       reportBurnIn = TRUE, nClusInit = 1000)

globalParsTrace(runInfoObj, parameters = "nClusters", plotBurnIn = TRUE)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
rain <- grep("rain",contVars, value = TRUE)
plotRiskProfile(riskProfObj, "summary.png", whichCovariates = rain)

clustering <- riskProfObj$riskProfClusObj$clustering
df3 <- cbind(df2, clustering)
df3 %>% select(1:4, clustering) %>% filter(clustering == 2)
df2 <- df %>% filter( !(Pedi == "WF9/H95" & StatID == 8657 & Repl == 2))