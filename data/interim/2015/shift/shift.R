library(tidyverse)
library(PReMiuM)
library(tictoc)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/shift/output")

df <- read_rds("../../hybrid_by_month_shift.rds")

contVars <- names(df)[grep("dew",names(df))]

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 2000, nBurn = 1000,
                       nProgress = 50, seed = 1234, 
                       reportBurnIn = TRUE, nClusInit = 1000)

globalParsTrace(runInfoObj, parameters = "alpha", plotBurnIn = TRUE)
write_rds(runInfoObj, path = "../runInfoObj.rds", compress = "xz")
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
plotRisk
write_rds(riskProfObj, "../riskProfObj.rds", compress = "xz")


