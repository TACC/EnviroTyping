library(PReMiuM)
library(tidyverse)

setwd("~/github/EnviroTyping/sandbox/shifted_data_analysis/2015/min_vars_1000/output")

df <- read_rds("../../../../../data/interim/2015/hybrid_by_month_shift_all_stats.rds")

variance.var <- names(which(map_dbl(df[,17:189], var, na.rm = TRUE) != 0))
mean.vars <- str_subset(variance.var, "Mean")

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = mean.vars, data = df, nSweeps = 1000, nBurn = 50, nProgress = 100, nClusInit = 1000, seed = 5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")

